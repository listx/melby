# Copyright 2024 Linus Arver
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

defmodule MelbydTest do
  use ExUnit.Case
  require Logger
  require WaitForIt

  setup_all do
    IO.puts("resetting fake_kube_config")
    content = "current-context=one\ncurrent-namespace=default\n"
    {:ok, mix_root} = File.cwd()
    path = mix_root <> "/test/fake_kube_config"
    File.write(path, content)
    :ok
  end

  # Generate a unique-looking 7-digit shell pid based on the module and test
  # name. This is important because each test must use a unique shell pid. The
  # alternative to using this helper function is to maually assign a unique
  # shell pid for each test case, which is error-prone.
  defp get_unique_shell_pid(module_name_atom, test_name_atom) do
    module_test_str = Atom.to_string(module_name_atom) <> Atom.to_string(test_name_atom)
    pid_length = 7

    :crypto.hash(:md5, module_test_str)
    |> :binary.decode_unsigned()
    |> Integer.to_string()
    |> String.graphemes()
    |> Enum.drop_while(fn c -> c == "0" end)
    |> Enum.take(pid_length)
    |> Enum.join()
    |> String.pad_trailing(pid_length, "0")
  end

  defp run_git_script(path, shell_script) do
    preamble = """
    set -euo pipefail
    clock_time="Thu, 07 Apr 2005 15:13:13 -0700"
    export GIT_AUTHOR_DATE="${clock_time}"
    export GIT_AUTHOR_NAME=a
    export GIT_AUTHOR_EMAIL=a@b.edu
    export GIT_COMMITTER_DATE="${clock_time}"
    export GIT_COMMITTER_NAME=d
    export GIT_COMMITTER_EMAIL=e@f.edu
    export GIT_CONFIG_GLOBAL=1
    export GIT_CONFIG_NOSYSTEM=1
    """

    # Run shell script at the given path. The stderr_to_stdout is so that we
    # capture all results into the "output" variable, to be sent to the
    # Logger.debug. Otherwise, the stderr output is not captured and always
    # displayed, going against the intent of `import ExUnit.CaptureLog` above.
    {output, ret} =
      System.cmd("bash", ["-c", preamble <> shell_script], cd: path, stderr_to_stdout: true)

    Logger.debug("got bash output: #{inspect(output)}")

    assert ret == 0
  end

  defp mk_tmp_dir(prefix) do
    Temp.track!()

    {:ok, dir_path} = Temp.mkdir(prefix)
    # On Darwin, /tmp is symlinked to /private/tmp. Temp.mkdir() (and even
    # Elixir's System.temp_dir()) uses "/tmp" as the temp directory. However,
    # this doesn't stop other Elixir libraries or even our own code (shelling
    # out to external commands) from using the non-symlinked version
    # "/private/tmp". So, make ourselves also use the dereferenced
    # (non-symlinked) path for consistency.
    Logger.warning("dir_path #{dir_path}")
    if :os.type() == {:unix, :darwin} do
      "/private" <> dir_path
    else
      dir_path
    end
  end

  # Initialize state (AGENT)
  defp init_lua_reader() do
    Agent.start_link(fn -> "" end)
  end

  # Read state (CONSUMER)
  defp get_lua_reader_state(pid) do
    Agent.get(pid, & &1)
  end

  # Update state (PRODUCER)
  defp update_lua_reader_state(pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed) do
    case Melbyd.LuaInterop.run(lua_script, ["Config", "view"], [env_vars, shell_pid]) do
      {:ok, [got]} ->
        Agent.update(pid, fn _ ->
          if ansi_escapes_allowed do
            got
          else
            strip_ansi(got)
          end
        end)

      x ->
        Logger.error("failed to run lua script: #{x}")
    end
  end

  defp expect(want, pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed) do
    update_lua_reader_state(pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed)
    get_lua_reader_state(pid) =~ want
  end

  # FIXME: Change argument order to match assert_output_substrings.
  defp assert_output_string(want, shell_pid, lua_script, env_vars, ansi_escapes_allowed) do
    {:ok, pid} = init_lua_reader()

    # Wait up to 5 seconds (5 poll events) to see if we can get some output that
    # has our =want= substring in it.
    WaitForIt.wait(
      expect(want, pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed),
      frequency: 1000,
      timeout: 5_000
    )

    got = get_lua_reader_state(pid)
    Agent.stop(pid)
    assert got =~ want
  end

  defp strip_ansi(s) do
    # This is a terrible regex, but performs adequately because our ANSI codes
    # are well-formed.
    Regex.replace(~r/\e\[([0-9;])*m/, s, "")
  end

  defp assert_output_substrings(
         want_substrs,
         shell_pid,
         lua_script,
         env_vars,
         ansi_escapes_allowed
       ) do
    {:ok, pid} = init_lua_reader()

    # Wait for the last substring to appear.
    WaitForIt.wait(
      expect(List.last(want_substrs), pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed),
      frequency: 1000,
      timeout: 5_000
    )
    # Alternate: wait for all substrings to appear. But actually, this is broken because we run the lua script too many times (once for each substring); so then our backend will purge any already-broadcasted shell message. The right approach is to wait up to 5 seconds, and during that time do fetches 1x per second, and for each fetch we should check how many of the substrings have matched, and NOT re-check those already-matched substrings in the next iteration. That is, we should match as much as possible each time we run the lua script and fetch (this way different parts of the output string can match what we want overall).
    #WaitForIt.wait(
    #  Enum.map(
    #    want_substrs,
    #    fn s -> expect(s, pid, shell_pid, lua_script, env_vars, ansi_escapes_allowed)
    #    end) |> Enum.all?(),
    #  frequency: 1000,
    #  timeout: 5_000
    #)

    # Run the Lua script to simulate invoking melbyc.
    got = get_lua_reader_state(pid)
    Agent.stop(pid)

    # Check every substring, not just the last one.
    # FIXME: Make this stricter by consuming the input in order (the substrings
    # must be found in order).
    Enum.each(want_substrs, fn substr ->
      assert String.contains?(got, substr)
    end)
  end

  # This tag lets us do "mix test --only basic" to only run this test.
  @tag basic: true
  test "basic smoke test (no git repo)", %{test: test_name} do
    shell_pid = get_unique_shell_pid(__MODULE__, test_name)

    {:ok, mix_root} = File.cwd()
    lua_script = mix_root <> "/test/sample/melby.lua"

    env_vars = %{
      KUBECONFIG: mix_root <> "/test/fake_kube_config",
      HOME: mix_root,
      # This path doesn't exist, but it doesn't really matter for purposes of
      # testing the path shortening logic.
      PWD: mix_root <> "/go/src/k8s.io/kubernetes",
      USER: "foo",
      HOST: "laptop",
      MELBY_UNIX_SECONDS: "1234567890",
      MELBY_TIME_ZONE: "America/Los_Angeles",
      MELBY_ZSH_KEYMAP_INDICATOR: "N",
      MELBY_PATH_ALIASES_FILE: mix_root <> "/test/sample/path-aliases",
      MELBY_DIR: mix_root <> "/test/sample"
    }

    assert_output_substrings(
      [
        "MELBY_PS1_LINE1",
        "\e[38;2;0;0;0;48;2;255;215;0;1m",
        "02-13 金 15:31:30 PST",
        "\e[38;2;0;0;0;48;2;255;192;203;1m",
        "02-13 金 23:31:30 UTC",
        "\e[0m\e[38;2;0;0;0;48;2;135;206;250;1m",
        "<N>",
        "foo@laptop",
        "\e[38;2;0;255;255;1m",
        "~kk",
        "MELBY_PS1_LINE2",
        "declare -a MELBY_SHELL_MESSAGES"
      ],
      shell_pid,
      lua_script,
      env_vars,
      true
    )
  end

  @tag ttl: true
  test "SRS ttl: exit when ttl reaches 0" do
    path_local = mk_tmp_dir("repo_local")

    run_git_script(path_local, """
    git init
    echo world > hello
    git add hello
    git commit -m 'initial import'
    """)

    {:ok, mix_root} = File.cwd()

    # Because the production code always sets the notify_on_exit_pid to nil (in
    # the normal codepath for users), we have to use a different code path. This
    # is essentially the body of the read_standard_resource Melbyd SDK function
    # we provide, but customized to not run Melbyd.StandardResource.read/2, but
    # to use our own logic to directly start the Melbyd.StandardResource
    # GenServer.
    {:ok, [resource_ref], st0} = Melbyd.LuaInterop.run_file(mix_root <> "/test/sample/Git.lua")

    resource = Melbyd.LuaSdk.resource_ref_to_native_map(resource_ref, st0)

    {:ok, _} =
      GenServer.start_link(Melbyd.StandardResource, %{
        id: {"Git", path_local},
        resource: resource,
        resource_opts: %{"PWD" => path_local, "fake" => false},
        ttl: 1,
        notify_on_exit_pid: self()
      })

    # FIXME: Move this 10_000 timeout to some global timeout (Application.get_env).
    assert_receive :shutting_down, 10_000
  end

  # We can't use :tmp_dir because we need multiple temporary directories.
  @tag git: true
  test "basic smoke test (with git repo)", %{test: test_name} do
    shell_pid = get_unique_shell_pid(__MODULE__, test_name)

    {:ok, mix_root} = File.cwd()

    # Create basic Git repo. We use a temporary folder for this.
    path_upstream = mk_tmp_dir("repo_upstream")

    run_git_script(path_upstream, """
    git init
    echo world > hello
    git add hello
    git commit -m 'initial import'
    """)

    path_local = mk_tmp_dir("repo_local")

    run_git_script(path_local, """
    cd /
    git clone --no-hardlinks #{path_upstream} #{path_local}
    """)

    env_vars = %{
      KUBECONFIG: mix_root <> "/test/fake_kube_config",
      HOME: "/home/foo",
      PWD: path_local,
      USER: "foo",
      HOST: "laptop",
      MELBY_UNIX_SECONDS: "1234567890",
      MELBY_TIME_ZONE: "America/Los_Angeles",
      MELBY_ZSH_KEYMAP_INDICATOR: "N",
      MELBY_PATH_ALIASES_FILE: mix_root <> "/test/sample/path-aliases",
      MELBY_DIR: mix_root <> "/test/sample"
    }

    path_local_short =
      Melbyd.Nifs.path_shorten(
        path_local,
        %{},
        %{"HOME" => "/home/foo"},
        30
      )

    lua_script = mix_root <> "/test/sample/melby.lua"

    # This initial response should not have Git information because it's too
    # quick (the Git watcher would have returned an initial empty loading
    # state). So check for the "[ Git... ]" string.
    assert_output_string("[ Git... ]", shell_pid, lua_script, env_vars, false)

    # After some time, the original "[ Git... ]" string should have disappeared
    # from the output by now, and we should have a final view like below.
    assert_output_substrings(
      [
        "MELBY_PS1_LINE1",
        "$(cat << 'END_HEREDOC'\n 02-13 金 15:31:30 PST   02-13 金 23:31:30 UTC   ",
        "<N>  foo@laptop #{path_local_short}\nEND_HEREDOC",
        "\n)\nMELBY_PS1_LINE2=$(cat << 'END_HEREDOC'\n[",
        "⎈ one:default 8T 5p 2r 0s 0f 1u]",
        " [ 3b83e1cd  master 19Y a]\nEND_HEREDOC\n)\ndeclare -a MELBY_SHELL_MESSAGES",
        "\nMELBY_SHELL_MESSAGES=(\n\n)\n",
      ],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    # If we make some changes to it, we should see it reflected. We also only
    # test for changes to the prompt that we expect to see --- namely the Git
    # parts.
    run_git_script(path_local, """
    echo bye >> hello
    """)

    assert_output_substrings(
      ["[ 3b83e1cd  master 19Y a U1+1]"],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_local, """
    git add hello
    """)

    assert_output_substrings(
      ["[ 3b83e1cd  master 19Y a S1+1]", "Staged size is now XS (4 bytes)."],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_local, """
    git commit -m 'append bye'
    """)

    assert_output_string(
      "[ f679b84c  master 19Y a ▲1]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_upstream, """
    echo x >> x
    git add x
    git commit -m 'upstream change 1'
    """)

    run_git_script(path_local, """
    git fetch
    """)

    assert_output_string(
      "[ f679b84c  master 19Y a ▲1 ▼1]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_local, """
    echo untracked > untracked
    """)

    assert_output_string(
      "[ f679b84c  master 19Y a ▲1 ▼1 N1]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_local, """
    echo 0 >> hello
    git stash
    """)

    assert_output_string(
      "[ f679b84c  master 19Y a ▲1 ▼1 N1 T1]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_local, """
    echo 2 > 2
    git add 2
    git commit -m 'local commit 2'
    echo x >> 2
    git update-index --assume-unchanged 2
    """)

    assert_output_string(
      "[ 2e4f8685  master 19Y a ▲2 ▼1 N1 T1 A1]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    path_sm1 = mk_tmp_dir("sm1")

    # Add submodule, but don't initialize it.
    run_git_script(path_sm1, """
    git init
    echo sm1 > sm1
    git add sm1
    git commit -m 'initial import'
    """)

    # Due to Git CVE-2022-39253, we hae to pass in "-c
    # protocol.file.allow=always" in order to allow adding a submodule from
    # disk. See https://bugs.launchpad.net/ubuntu/+source/git/+bug/1993586.
    #
    # Also, because the absolute path on disk is unique in each run, it appears
    # that the resulting commit SHA will also be different on each run of this
    # test. To get around this, we don't check for the SHA any more.
    run_git_script(path_upstream, """
    git -c protocol.file.allow=always submodule add #{path_sm1} sm1
    git commit -m 'add submodule'
    """)

    run_git_script(path_local, """
    git update-index --no-assume-unchanged 2
    git checkout 2
    git pull --rebase
    """)

    assert_output_string(
      "master 19Y a ▲2 N1 T1 M{not_init=1}]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    path_sm2 = mk_tmp_dir("sm2")

    run_git_script(path_sm2, """
    git init
    echo sm2 > sm2
    git add sm2
    git commit -m 'initial import'
    """)

    run_git_script(path_upstream, """
    git -c protocol.file.allow=always submodule add #{path_sm2} sm2
    git commit -m 'add submodule sm2'
    """)

    run_git_script(path_local, """
    git pull --rebase
    git -c protocol.file.allow=always submodule update --init sm2
    """)

    run_git_script(path_sm2, """
    echo foo > foo
    git add foo
    git commit -m 'add foo'
    """)

    run_git_script(path_upstream, """
    cd sm2
    git pull
    cd ..
    git add --update
    git commit -m 'update submodule sm2'
    """)

    run_git_script(path_local, """
    git pull --rebase
    """)

    # The "C•1+1-1" is because the local repo's sm2 is still checked out at an
    # older version, whereas upstream has moved on to a newer commit.
    assert_output_string(
      "master 19Y a ▲2 U1+1-1 N1 T1 M{not_init=1 need_sync=1}]",
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    run_git_script(path_sm2, """
    echo there > there
    git add there
    git commit -m 'add there'
    """)

    run_git_script(path_upstream, """
    cd sm2
    git pull
    cd ..
    git add sm2
    git commit -m 'use sm2 master'
    """)

    # If we try to merge two branches of the superproject together but they
    # reference different versions of a submodule, this should result in a
    # submodule merge conflict.
    run_git_script(path_local, """
    cd sm2
    git rm -f sm2
    echo hello > sm2
    git add sm2
    git commit -m 'changing sm2 locally'

    cd ..
    git add sm2
    git commit -m 'using local sm2 update'
    git fetch
    git merge origin/master || true
    """)

    assert_output_string(
      "master 19Y a ▲3 ▼1 N1 T1 M{not_init=1 conflict=1}",
      shell_pid,
      lua_script,
      env_vars,
      false
    )
  end

  @tag k8s: true
  test "basic smoke test (with kubectl)", %{test: test_name} do
    shell_pid = get_unique_shell_pid(__MODULE__, test_name)

    {:ok, mix_root} = File.cwd()
    lua_script = mix_root <> "/test/sample/melby.lua"

    non_git_repo = mk_tmp_dir("non_git_repo")

    content = "current-context=one\ncurrent-namespace=default\n"
    kubeconfig = non_git_repo <> "/fake_kube_config"
    File.write(kubeconfig, content)

    env_vars = %{
      KUBECONFIG: kubeconfig,
      HOME: mix_root <> "/test/sample",
      PWD: non_git_repo,
      USER: "foo",
      HOST: "laptop",
      MELBY_UNIX_SECONDS: "1234567890",
      MELBY_TIME_ZONE: "America/Los_Angeles",
      MELBY_ZSH_KEYMAP_INDICATOR: "N",
      MELBY_PATH_ALIASES_FILE: mix_root <> "/test/sample/path-aliases",
      MELBY_DIR: mix_root <> "/test/sample"
    }

    assert_output_substrings(
      [
        "[ K8s... ]"
      ],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    assert_output_substrings(
      [
        "one:default"
      ],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    # Change config to point to "two:bar".
    content = "current-context=two\ncurrent-namespace=bar\n"
    File.write(kubeconfig, content)

    assert_output_substrings(
      [
        "Context changed from 'one' to 'two'.",
        "Namespace changed from 'default' to 'bar'."
      ],
      shell_pid,
      lua_script,
      env_vars,
      false
    )

    assert_output_substrings(
      [
        "two:bar"
      ],
      shell_pid,
      lua_script,
      env_vars,
      false
    )
  end
end
