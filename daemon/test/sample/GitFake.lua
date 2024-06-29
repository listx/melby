-- Copyright 2024 Linus Arver
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

local GitFake = {}

-- The return format is a list of tables where each element is a {input="..."",
-- output={...kvs...}}.
function diff_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- Some file changed.
      input = " 1 file changed, 35 insertions(+)"
      output["unstaged_files"] = 1
      output["unstaged_insertions"] = 35
      output["unstaged_deletions"] = 0
    elseif time_idx == 1 then
      -- Some file was changed again.
      input = " 1 file changed, 32 insertions(+), 2 deletions(-)"
      output["unstaged_files"] = 1
      output["unstaged_insertions"] = 32
      output["unstaged_deletions"] = 2
    end
  end

  return {input=input, output=output}
end

function diff_cached_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- Some file changed.
      input = " 2 files changed, 4 insertions(+)"
      output["staged_files"] = 2
      output["staged_insertions"] = 4
      output["staged_deletions"] = 0
    elseif time_idx == 1 then
      -- Some file was changed again.
      input = " 2 files changed, 4 insertions(+), 8 deletions(-)"
      output["staged_files"] = 2
      output["staged_insertions"] = 4
      output["staged_deletions"] = 8
    else
      output["staged_files"] = 0
      output["staged_insertions"] = 0
      output["staged_deletions"] = 0
    end
  end

  return {input=input, output=output}
end

function diff_binary_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- No binary files changed.
      input = [[
5   1   doc/build-literate.org
]]
      output["unstaged_files_binary"] = 0
    elseif time_idx == 1 then
      -- One binary file changed.
      input = [[
5   1   doc/build-literate.org
-   -   doc/melby.html
]]
      output["unstaged_files_binary"] = 1
    else
      -- Two binary files changed.
      input = [[
5   1   doc/build-literate.org
-   -   doc/melby.html
-   -   foo
]]
      output["unstaged_files_binary"] = 2
    end
  end

  return {input=input, output=output}
end

function diff_cached_binary_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- One binary file changed.
      input = [[
5   1   apple
-   -   foo
]]
      output["staged_files_binary"] = 1
    elseif time_idx == 1 then
      -- No binary files changed.
      input = [[
5   1   apple
]]
      output["staged_files_binary"] = 0
    else
      -- Three binary files changed.
      input = [[
5   1   apple
-   -   foo
-   -   bar
-   -   baz
]]
      output["staged_files_binary"] = 3
    end
  end

  return {input=input, output=output}
end

function staged_bytes_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- No staged bytes.
      input = "0"
      output["staged_bytes"] = 0
    elseif time_idx == 1 then
      input = "12"
      output["staged_bytes"] = 12
    else
      input = "500"
      output["staged_bytes"] = 500
    end
  end

  return {input=input, output=output}
end

function head_metadata_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    input = [[
head_author_date,1678169218
head_author_name,Foo Bar
head_committer_date,1678169218
head_committer_name,Foo Bar
]]
    output["head_author_date"] = 1678169218
    output["head_author_name"] = "Foo Bar"
    output["head_committer_date"] = 1678169218
    output["head_committer_name"] = "Foo Bar"
  end

  return {input=input, output=output}
end

function untracked_files_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    input = [[
file1
]]
    output["untracked_files"] = 1
  end

  return {input=input, output=output}
end

function assume_unchanged_files_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      input = [[
H apple
h carrot
]]
      output["assume_unchanged_files"] = 1
    else
      input = [[
H apple
H carrot
]]
      output["assume_unchanged_files"] = 0
    end
  else
    input = [[
H f1
]]
    output["assume_unchanged_files"] = 0
  end

  return {input=input, output=output}
end

function count_head_vs_upstream_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      input = [[
0   0
]]
      output["count_upstream_to_head"] = 0
      output["count_head_to_upstream"] = 0
    elseif time_idx == 1 then
      -- We created a commit.
      input = [[
1   0
]]
      output["count_upstream_to_head"] = 1
      output["count_head_to_upstream"] = 0
    else
      -- Upstream has moved on (our local branch has diverged).
      input = [[
1   1
]]
      output["count_upstream_to_head"] = 1
      output["count_head_to_upstream"] = 1
    end
  end

  return {input=input, output=output}
end

function head_branch_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    input = "main"
    output["head_branch"] = "main"
  else
    input = "dev"
    output["head_branch"] = "dev"
  end

  return {input=input, output=output}
end

function head_sha_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    input = "2bf048d6364fff6f80d0f96f11447a80859f0df7"
    output["head_sha"] = "2bf048d6364fff6f80d0f96f11447a80859f0df7"
  else
    input = "6ef4740cfe770f68af432fa27e0dc693dab8fce5"
    output["head_sha"] = "6ef4740cfe770f68af432fa27e0dc693dab8fce5"
  end

  return {input=input, output=output}
end

function bare_fake(pwd, time_idx)
  local input = ""
  local output = {}

  input = "false"
  output["bare"] = false

  return {input=input, output=output}
end

function stash_size_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      output["stash_size"] = 0
    elseif time_idx == 1 then
      output["stash_size"] = 0
    else
      -- Stashed some changes.
      input = [[
stash@{0}: WIP on main: 6ef4740 update
]]
      output["stash_size"] = 1
    end
  end

  return {input=input, output=output}
end

function submodule_status_fake(pwd, time_idx)
  local input = ""
  local output = {}

  if pwd == "/repo/a" then
    if time_idx == 0 then
      -- Nothing initialized.
      input = [[
-e827a9f3e2ba585de8b07cd246534a15ec414a8b deps/elisp/compat.el (28.1.2.2-7-ge827a9f)
-0ac1ecf6b56eb67bb81a3cf70f8d4354b5782341 deps/elisp/dash.el (2.19.1-20-g0ac1ecf)
-d495ed87a9c507f5939a51c740f119950c83e2ff deps/elisp/emacs-elixir (v2.3.1-135-gd495ed8)
-dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9 deps/elisp/emacs-htmlize (release/1.56-1-gdd27bc3)
-90503413f4cdb0ed26871e39c4e6e2552b57f7db deps/elisp/haskell-mode (v13.14-1398-g9050341)
-d17a00ca50aee197cd017d573b83367eb241cc44 deps/elisp/lua-mode (v20210802-4-gd17a00c)
-8158b484ae32d334c9db60b265759d22547ea952 deps/elisp/magit (v3.3.0-462-g8158b484)
-34d51e2731408b5b615f785a83faa3d6dc2a92a1 deps/elisp/nix-mode (v1.4.1-194-g34d51e2)
-061cfc002ff6ea41c622447bec22f49d618c36de deps/elisp/org-html-themify (heads/master)
-f6e284f8e5dd557fde52241546a916c2e50f0d23 deps/elisp/protobuf (v3.21.6-629-gf6e284f8e)
-b4537b6f5fa65626c1bab944681b35769cab9b5c deps/elisp/rust-mode (1.0.5-18-gb4537b6)
-e957dcb0677da18b2bb60ad867db5df5c35b5616 deps/elisp/s.el (1.12.0-71-ge957dcb)
-535800fd6ca7f5af56f7aa3d0e8f46fef8b7999b deps/elisp/themes (v2.3.0-8-g535800f)
-3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7 deps/elisp/yaml-mode (release-0.0.6-132-g3fcb36d)
-d8cf56bb1fa0280fe5b1a2b335dd6637f1964851 deps/misc/org-html-themes (v1.0.2-88-gd8cf56b)
]]
      output["submodule_uninitialized"] = 15
      output["submodule_out_of_sync"] = 0
      output["submodule_merge_conflicts"] = 0
    else
      -- Everything has initialized.
      input = [[
 e827a9f3e2ba585de8b07cd246534a15ec414a8b deps/elisp/compat.el (28.1.2.2-7-ge827a9f)
 0ac1ecf6b56eb67bb81a3cf70f8d4354b5782341 deps/elisp/dash.el (2.19.1-20-g0ac1ecf)
 d495ed87a9c507f5939a51c740f119950c83e2ff deps/elisp/emacs-elixir (v2.3.1-135-gd495ed8)
 dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9 deps/elisp/emacs-htmlize (release/1.56-1-gdd27bc3)
 90503413f4cdb0ed26871e39c4e6e2552b57f7db deps/elisp/haskell-mode (v13.14-1398-g9050341)
 d17a00ca50aee197cd017d573b83367eb241cc44 deps/elisp/lua-mode (v20210802-4-gd17a00c)
 8158b484ae32d334c9db60b265759d22547ea952 deps/elisp/magit (v3.3.0-462-g8158b484)
 34d51e2731408b5b615f785a83faa3d6dc2a92a1 deps/elisp/nix-mode (v1.4.1-194-g34d51e2)
 061cfc002ff6ea41c622447bec22f49d618c36de deps/elisp/org-html-themify (heads/master)
 f6e284f8e5dd557fde52241546a916c2e50f0d23 deps/elisp/protobuf (v3.21.6-629-gf6e284f8e)
 b4537b6f5fa65626c1bab944681b35769cab9b5c deps/elisp/rust-mode (1.0.5-18-gb4537b6)
 e957dcb0677da18b2bb60ad867db5df5c35b5616 deps/elisp/s.el (1.12.0-71-ge957dcb)
 535800fd6ca7f5af56f7aa3d0e8f46fef8b7999b deps/elisp/themes (v2.3.0-8-g535800f)
 3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7 deps/elisp/yaml-mode (release-0.0.6-132-g3fcb36d)
 d8cf56bb1fa0280fe5b1a2b335dd6637f1964851 deps/misc/org-html-themes (v1.0.2-88-gd8cf56b)
]]
      output["submodule_uninitialized"] = 0
      output["submodule_out_of_sync"] = 0
      output["submodule_merge_conflicts"] = 0
    end
  end

  return {input=input, output=output}
end

-- Note that this invokes the fake readers when it is called, so the values in
-- the table are already the generated {input=..., output=...} tables. This is
-- slightly different than the real readers where the caller still needs to run
-- System.cmd to generate the read results by shelling out.
function GitFake.readers (resource_opts, time_idx)
  local pwd = resource_opts["PWD"]
  return {
    diff = diff_fake(pwd, time_idx),
    diff_cached = diff_cached_fake(pwd, time_idx),
    diff_binary = diff_binary_fake(pwd, time_idx),
    diff_cached_binary = diff_cached_binary_fake(pwd, time_idx),
    staged_bytes = staged_bytes_fake(pwd, time_idx),
    head_metadata = head_metadata_fake(pwd, time_idx),
    untracked_files = untracked_files_fake(pwd, time_idx),
    assume_unchanged_files = assume_unchanged_files_fake(pwd, time_idx),
    count_head_vs_upstream = count_head_vs_upstream_fake(pwd, time_idx),
    head_branch = head_branch_fake(pwd, time_idx),
    head_sha = head_sha_fake(pwd, time_idx),
    bare = bare_fake(pwd, time_idx),
    stash_size = stash_size_fake(pwd, time_idx),
    submodule_status = submodule_status_fake(pwd, time_idx),
  }
end

function GitFake.resource_id_func (resource_opts)
  local pwd = resource_opts["PWD"]

  if string.find(pwd, "/repo/a") then
    return "/repo/a"
  end

  return ""
end

return GitFake
