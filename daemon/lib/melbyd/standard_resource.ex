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

defmodule Melbyd.StandardResource do
  # If a Standard Resource Service process dies, don't restart it, because it
  # will be restarted on the next use (when a client requests info for it).
  use GenServer, restart: :temporary
  require Logger

  # 1 second. This is how quickly we can call the various reader functions
  # (which can be expensive). If we detect some possibility of staleness, we
  # re-execute the reader functions to retrieve the new state. If no staleness
  # is detected, then nothing happens in this tick.
  #
  # In the case where staleness is detected, we run the "read" function which
  # re-reads state. After this function complete, we run tick() again. So the
  # "busiest" we can be is tick() -> read() -> tick() -> read(), and so on. Or,
  # more realistically, it will be tick() -> tick() -> tick() -> read() ->
  # tick(), where there are multiple (possibly tens or hundreds) of ticks before
  # we need to call read().
  @tick_interval 1000

  # This function is called to initiate a brand new process. The most important
  # part here is the id, which is passed into via_tuple/1 to generate a tuple
  # that can be used to look up this process in a globally unique way. This way
  # we can send messages to *existing* processes and work with them.

  # Note that we pass the args_for_init tuple as the second argument to
  # GenServer.start_link/3, which in turn runs the init/1 callback by passing in
  # this same second argument.

  # An SRS instance is started by a gRPC call from melbyc, because only melbyc
  # knows which repo_id path to use (using "git"-flavored SRS as an example).
  # Because melbyc is the entrypoint, we should store all Lua config for SRS in
  # the same config used by the user for prompt generation and widget rendering.
  def start_link(
        %{
          id: {_resource_type, _resource_id} = id,
          resource: _resource,
          resource_opts: _resource_opts,
          ttl: _ttl,
          notify_on_exit_pid: _notify_on_exit_pid
        } = args_for_init
      ) do
    GenServer.start_link(__MODULE__, args_for_init, name: via_tuple(id))
  end

  # Used to identify this process uniquely in the entire Elixir system. We use
  # the gproc library for this.
  defp via_tuple(id) do
    key = {:n, :l, {__MODULE__, id}}
    {:via, :gproc, key}
  end

  @impl true
  def init(
        %{
          id: id,
          resource: resource,
          resource_opts: resource_opts,
          ttl: ttl,
          notify_on_exit_pid: notify_on_exit_pid
        } = _args
      ) do
    # Trap exits so that we can terminate gracefully (such as not logging an
    # error due to an intentional shutdown).
    #
    # Note that this will result in receiving all exits from all other linked
    # processes --- in our case this happens rather frequently from our use of
    # System.cmd/3 when the shell processes we start exit normally after
    # finishing execution (with a ":normal" reason).
    Process.flag(:trap_exit, true)

    Logger.info("Starting SRS #{inspect(id)}; ttl=#{inspect(ttl)}")

    # We have to subscribe to the fake_* topic ASAP (in init/1 here, not in
    # handle_continue/2), because it may be the case that handle_continue/2 will
    # not finish running by the time we broadcast the message to shut down this
    # GenServer on this fake_* topic.
    if resource_opts["fake"] do
      Logger.debug("#{inspect(id)}: subscribing to fake_" <> resource_opts["vm_fingerprint"])
      # Listen to the topic dedicated for all fakes created for a particular
      # vm_fingerprint. Then later when we're done with using this fake
      # (validation phase is over), we can delete all fakes by sending a message
      # here.
      Phoenix.PubSub.subscribe(Melbyd.PubSub, "fake_" <> resource_opts["vm_fingerprint"])
    end

    initial_state = %{
      id: id,
      resource: resource,
      resource_opts: resource_opts,
      state_hist: [],
      # This is stale because we haven't read any information yet.
      stale: true,
      ttl: ttl,
      ttl_max: ttl,
      notify_on_exit_pid: notify_on_exit_pid,
      reads: 0
    }

    # Return quickly, so that we don't block the creation of this GenServer. We
    # handle additional setup in the handle_continue/2 below.
    {:ok, initial_state, {:continue, :initial_read}}
  end

  @impl true
  def handle_continue(
        :initial_read,
        %{
          id: id,
          resource: resource,
          resource_opts: resource_opts,
          ttl: ttl,
          ttl_max: ttl,
          notify_on_exit_pid: notify_on_exit_pid,
          reads: reads
        } = state
      ) do
    {resource_type, resource_id} = id

    # Do an initial read to populate state. Note that this assumes that melbyd is
    # running on the same machine as the client.
    t1 = Timex.local()
    state_current = run_readers(resource, resource_opts, reads)
    t2 = Timex.local()

    # Give diagnostic report about how long it took to run all the commands to
    # generate the initial state.
    seconds_float = DateTime.diff(t2, t1, 10) / 10

    message = %{
      topic: "srs_#{resource_type}",
      from: "#{resource_id}",
      payload: %{
        level: "info",
        time: Calendar.strftime(t2, "%H:%M:%S"),
        text: "Initial read took #{seconds_float}s."
      }
    }

    Phoenix.PubSub.broadcast(Melbyd.PubSub, "srs_" <> resource_type, message)

    # History of states to store. This could be 1 or 100, depending on how much
    # recency data we want to retain. It may be that we want to store 100
    # (unique) states because we want to keep short-term-memory that we want to
    # retrieve, such as "what are the names of all git branches I have checked
    # out in the current session?" to be able to switch to them easily without
    # having to remember the exact names. Another example is providing users a
    # list of all unique SHAs that were visited in the current session.
    state_hist = [state_current]

    new_state = %{
      state
      | state_hist: state_hist,
        stale: false,
        reads: reads + 1
    }

    # Activate staleness detectors. For the FileSystem, we set up an
    # fs_event_handler.
    #
    # For fake resources, we skip setting up filesystem-based staleness
    # detection because we want to stop SRS from reading state on its own
    # initiative. Instead for fake resources, their states should only be
    # refreshed when the caller calls read().
    new_state =
      if resource_opts["fake"] do
        # For "fake" resources, skip filesystem flaggers because we could be
        # working with fake filesystem folders that don't actually exist. In
        # exchange, it's up to the rest of the fake handling code to understand
        # how to make up for this (make the user send in fake filesystem events
        # that SRS can still react to).
        new_state
      else
        # FIXME: Move all of this to a "setup_staleness_flaggers" function.
        [staleness_flaggers_luerl_array] = resource["staleness_flaggers"].([resource_id])

        staleness_flaggers_luerl_tables =
          Melbyd.LuerlUtil.array_to_native_list(staleness_flaggers_luerl_array)

        staleness_flaggers =
          staleness_flaggers_luerl_tables |> Enum.map(&Melbyd.LuerlUtil.table_to_native_map/1)

        Enum.reduce(staleness_flaggers, new_state, &setup_staleness_flagger/2)
      end

    # Start up the tick process to detect staleness and subsequent re-reading of
    # state. Only do this if we are a real resource.
    if not resource_opts["fake"] do
      tick(ttl, notify_on_exit_pid)
    end

    {:noreply, new_state}
  end

  defp run_readers(resource, resource_opts, reads) do
    [readers_luerl_array] = resource["readers"].([resource_opts])
    readers = Melbyd.LuerlUtil.array_to_native_list(readers_luerl_array)

    kvs =
      if resource_opts["fake"] do
        # Run the fake readers instead of the real ones.

        # For the time_idx, we can use a new "reads" field in the state that
        # starts at 0 and is incremented by 1 every time that run_readers()
        # completes. We don't care about overflows because Elixir uses arbitrary
        # precision integers (and btw it'll take billions of years of continuous
        # incrementation until we need to use more than 64 bits so even if
        # Elixir did not use arbitrary precision, we would virtually never
        # overflow).

        time_idx = reads

        # The fake readers generate data. We still need the regular readers
        # because we need to know which readers need which parsers.
        [fake_readers_luerl_table] = resource["fake"]["readers"].([resource_opts, time_idx])
        fake_readers = Melbyd.LuerlUtil.table_to_native_map(fake_readers_luerl_table)
        case read_fakes(readers, resource, resource_opts, fake_readers, time_idx) do
          {[], kvs} ->
            kvs

          {errors, _kvs} ->
            Logger.warning("got #{Kernel.length(errors)} errors reading fake readers")
            errors |> Enum.map(fn e -> Logger.warning(e) end)
            # Still return those kvs that were successfully parsed as expected.
            # FIXME: Or should we not crash here?
            raise "failed validation"
        end
      else
        # FIXME: Optionally run the readers concurrently for cases where we do
        # want the concurrency (e.g., we're hitting N different IP targets over
        # the network). E.g., for kubernetes contexts, most likely we should run
        # them all in parallel because they are talking to different clusters
        # with different IPs.
        #
        # We should by default read serially because it is "safer". E.g., for
        # git we definitely do not want to run everything in parallel because
        # each git command could block on a global lockfile located in the .git
        # folder of a repo.
        if resource_opts["read_parallel"] do
          read_parallel(readers, resource)
        else
          read_serial(readers, resource)
        end
      end

    %MelbyDaemon.StandardResource{status: :STANDARD_RESOURCE_STATUS_LOADED, kvs: kvs}
  end

  # For fake readers, check that the parser's output matches the expected output
  # in the fake.
  defp read_fakes(readers, resource, resource_opts, fake_readers, time_idx) do
    Enum.reduce(readers, {[], %{}}, fn reader_table, {errors, kvs} ->
      reader = Melbyd.LuerlUtil.table_to_native_map(reader_table)
      fake_reader_table = fake_readers[reader["parser"]]
      fake_reader = Melbyd.LuerlUtil.table_to_native_map(fake_reader_table)
      parser_func = resource["parser"][reader["parser"]]
      src = fake_reader["input"]
      kvs_subset = parser_func.([src]) |> Kernel.hd() |> Melbyd.LuerlUtil.table_to_native_map()

      # If the output does not match what we actually parsed, raise an error.
      expected = fake_reader["output"] |> Melbyd.LuerlUtil.table_to_native_map()

      errors = if kvs_subset != expected do
        # FIXME: Maybe use a map diffing library, like
        # https://hexdocs.pm/map_diff/MapDiff.html to get a shorter "diff" of
        # any key/value differences.
        ["resource type #{resource["type"]}: " <>
          "resource_opts #{inspect(resource_opts)}: " <>
          "time_idx #{time_idx}: " <>
          "expected #{inspect(expected)}, got #{inspect(kvs_subset)}" | errors]
      else
        errors
      end

      # Merge the data we've collected into acc.
      {errors, Map.merge(kvs, kvs_subset)}
    end)
  end

  defp read_single(reader_table, resource) do
    reader = Melbyd.LuerlUtil.table_to_native_map(reader_table)
    invocation = Melbyd.LuerlUtil.array_to_native_list(reader["invocation"])
    cmd_head = Kernel.hd(invocation)
    cmd_args = Enum.drop(invocation, 1)
    cd = reader["cd"]

    cmd_opts_cd =
      if cd != nil do
        [cd: cd]
      else
        []
      end

    cmd_opts_env =
      if reader["env"] != nil do
        env =
          Melbyd.LuerlUtil.table_to_native_map(reader["env"])
          |> Enum.map(fn {k, v} -> {k, v} end)

        [env: env]
      else
        []
      end

    cmd_opts = cmd_opts_cd ++ cmd_opts_env

    parser_func_name = reader["parser"]
    parser_func = resource["parser"][parser_func_name]

    src =
      try do
        case System.cmd(cmd_head, cmd_args, cmd_opts) do
          {stdout, 0} ->
            stdout

          {_stdout, error_code} ->
            Logger.warning("Command #{inspect(invocation)} failed with error code #{error_code}")

            # Return empty string. Parsers should know how to deal with the
            # empty string (and accept that no output means that we should
            # return a default value).
            ""
        end
      rescue
        e ->
          Logger.warning("Command #{inspect(invocation)} failed: #{Exception.message(e)}")
          ""
      end

    Logger.debug("Running parser_func #{inspect(parser_func_name)}")

    # Now parse the output with the custom function to generate some keys and
    # values (a map). For simplicity both the keys and values here should not be
    # a collection type (map, array, etc) and instead be a primitive like a
    # string or number.
    parser_func.([src]) |> Kernel.hd() |> Melbyd.LuerlUtil.table_to_native_map()
  end

  defp read_serial(readers, resource) do
    Enum.reduce(readers, %{}, fn reader_table, acc ->
      kvs_subset = read_single(reader_table, resource)
      # Merge the data we've collected into acc.
      Map.merge(acc, kvs_subset)
    end)
  end

  # async tasks can crash the caller (and vice versa)
  # https://hexdocs.pm/elixir/1.14.2/Task.html#module-async-and-await. Also,
  # note that this function may take minutes, or even hours, to return. But it's
  # OK because we're not blocking anyone else.
  defp read_parallel(readers, resource) do
    tasks =
      Enum.reduce(readers, [], fn reader_table, acc ->
        task = Task.async(fn -> read_single(reader_table, resource) end)
        # Collect this task.
        [task | acc]
      end)

    # Each task returns a map. We need to merge all of these maps into each
    # other.
    maps = Task.await_many(tasks, :infinity)
    Enum.reduce(maps, &Map.merge/2)
  end

  # FIXME: put this inside the callbacks section for handle_call.
  @impl true
  def handle_call(
        :read,
        _from,
        %{state_hist: state_hist, ttl: ttl, ttl_max: ttl_max} = state
      ) do
    response =
      case state_hist do
        [] -> %MelbyDaemon.StandardResource{status: :STANDARD_RESOURCE_STATUS_LOADING}
        [current | _] -> current
      end

    # If ttl is < 1, then this means that either the ttl naturally expired (ttl
    # == 0) or that we set this ttl manually to -1 because the fs watcher died.
    # In either case, do not change the ttl.
    #
    # Otherwise, reset the ttl because a client actually needed this
    # information.
    ttl_new =
      if ttl < 1 do
        ttl
      else
        ttl_max
      end

    {
      :reply,
      # Response to the caller.
      response,
      # New state of this GenServer.
      %{state | ttl: ttl_new}
    }
  end

  # GenServer callbacks.
  @impl true
  def handle_call(
        :tick,
        _from,
        state
      ) do
    new_state = maybe_refresh_state_and_notify(state)
  
    # Unlike for the handle_info version, we do not tick again on our own, because
    # this is meant to be used only as a way for fake resources to get updated
    # manually in a synchronized fashion. If we were to tick ourselves now, then
    # we would essentially start updating our state asynchronously, missing the
    # point.
    {:reply, :ok, new_state}
  end
  @impl true
  def handle_call(:mark_stale, _from, %{stale: _, state_hist: [current | rest]} = state) do
    {:reply,
     :ok,
     %{
       state
       | stale: true,
         state_hist: [%{current | status: :STANDARD_RESOURCE_STATUS_LOADING} | rest]
     }}
  end
  @impl true
  def handle_cast(:mark_stale, %{stale: _, state_hist: [current | rest]} = state) do
    {:noreply,
     %{
       state
       | stale: true,
         state_hist: [%{current | status: :STANDARD_RESOURCE_STATUS_LOADING} | rest]
     }}
  end
  # Process tick. The tick must handle the true and false cases for the "stale"
  # key of the state. First we handle the case where stale is true (we must
  # re-read data).
  defp maybe_refresh_state_and_notify(
         %{
           id: id,
           resource: resource,
           resource_opts: resource_opts,
           state_hist: state_hist,
           stale: stale,
           ttl: ttl,
           reads: reads
         } = state
       ) do
    if stale do
      Logger.info("Re-reading state for #{inspect(id)}")
      new = run_readers(resource, resource_opts, reads)
      Logger.info("Finished re-reading state for #{inspect(id)}")
  
      new_state_hist =
        case state_hist do
          # This list is always populated with at least 1 element because we
          # populate it as a singleton list in init/1.
          [old | _] ->
            if old == new do
              # NOP because there is no change between the currnt reading and the
              # last reading we did.
              Logger.info("skipping addition of new state; NOP")
              state_hist
            else
              # Generate any new messages for any diff between the old and new
              # states.
              Logger.info("checking for any new messages to broadcast")
              {_, resource_id} = id
              resource["notify"].([resource_id, old, new])
  
              # Drop oldest state from state_hist if adding (prepending) to it
              # would exceed our history size.
              [new | state_hist] |> Enum.take(resource["history_size"])
            end
        end
  
      %{state | state_hist: new_state_hist,
                stale: false,
                ttl: ttl - 1,
                reads: reads + 1}
    else
      # Now handle the case where staleness is false (no need to read new data).
      # In this case the only thing that happens is the ttl age getting older (1
      # unit closer to 0).
  
      %{state | ttl: ttl - 1}
    end
  end
  
  @impl true
  def handle_info(
        :tick,
        %{ttl: ttl, notify_on_exit_pid: notify_on_exit_pid} = state
      ) do
    new_state = maybe_refresh_state_and_notify(state)
  
    # Continue ticking for the future. But optionally die if ttl is too low.
    tick(ttl, notify_on_exit_pid)
  
    {:noreply, new_state}
  end
  @impl true
  def handle_info(
        {:file_event, _watcher_pid, {path, events}},
        %{
          id: id,
          fs_event_handler: fs_event_handler,
          stale: false
        } = state
      ) do
    {resource_type, _resource_id} = id
    # Now translate our path and events arguments to send into the Lua function,
    # and call it.
    [should_mark_stale] = fs_event_handler.([path, events])
  
    if should_mark_stale do
      # Invalidate the cache entry for all current and parent SRS GenServers
      # between / and path. This includes us (our particular SRS GenServer
      # instance) as well.
      mark_all_paths_stale_from({resource_type, path})
    else
      Logger.debug(
        "ignoring Git index path #{path}; events:#{inspect(events)}"
      )
    end
  
    {:noreply, state}
  end
  @impl true
  def handle_info(
        {:file_event, watcher_pid, :stop},
        %{
          id: id
        } = state
      ) do
    Logger.info("SRS id #{inspect(id)}, fs watcher #{inspect(watcher_pid)}: FileSystem monitor stopped")
  
    # FIXME: Use {:stop, reason, new_state} here to stop the process instead of
    # (ab)using ttl. See pages 174-175 of Elixir In Action.
    {:noreply, %{state | ttl: -1}}
  end
  @impl true
  def handle_info(
        {:file_event, _watcher_pid, {_path, _events}},
        %{
          stale: true
        } = state
      ) do
    Logger.debug("Ignoring filesystem event because state is already stale")
    {:noreply, state}
  end
  @impl true
  def handle_info(
        :duration_event,
        %{
          id: id,
          stale: false
        } = state
      ) do
    {resource_type, _resource_id} = id
    mark_stale({resource_type, id})
  
    {:noreply, state}
  end
  @impl true
  def handle_info(
        :duration_event,
        %{
          stale: true
        } = state
      ) do
    Logger.debug("Ignoring duration event because state is already stale")
    {:noreply, state}
  end
  @impl true
  def handle_info(
        {:EXIT, from_pid, reason},
        %{
          id: id
        } = state
      ) do
    Logger.debug("SRS #{inspect(id)}: Got exit reason #{inspect(reason)} from pid #{inspect(from_pid)}")
    case reason do
      :normal ->
        # This can happen if, e.g., a System.cmd/3 finishes running successfully.
        {:noreply, state}
        # This is when we are asked to shut down immediately (e.g., for a fake SRS
        # that is no longer needed).
      :release_fake_resource ->
        {:stop, :normal, state}
      :ttl_deadline_exceeded ->
        # Invoke our terminate/2 callback by returning with the ":stop" atom.
        {:stop, :normal, state}
      _ ->
        {:stop, reason, state}
    end
  end
  
  @impl true
  def terminate(
        reason,
        %{
          id: id
        } = _state
      ) do
    Logger.info("SRS #{inspect(id)}: Got exit reason #{inspect(reason)}; shutting down")
  end

  # Tick
  # Send a "tick" message to our GenServer in 1 second. See
  # https://stackoverflow.com/a/32097971/437583.
  defp tick(ttl, notify_on_exit_pid) do
    case ttl do
      n when n in -1..0 ->
        if n == 0 do
          Logger.info("TTL expired; shutting down this GenServer due to client neglect")
        else
          Logger.info(
            "TTL expired manually; shutting down this GenServer"
          )
        end
  
        # Used for testing, where we assert that we can receive this
        # ":shutting_down" message after the ttl expires.
        if notify_on_exit_pid do
          send(notify_on_exit_pid, :shutting_down)
        end
  
        Process.exit(self(), :ttl_deadline_exceeded)
      _ ->
        # Send after 1 second. We could alternatively use :timer.send_interval
        # (Erlang function) in init/1 and avoid calling this function manually in
        # handle_info/2, but then that would send the tick at a constant rate,
        # regardless of how long it takes to process the tick. This runs the risk of
        # growing the message queue at a faster rate than it can be processed
        # (unbounded growth).
        Process.send_after(self(), :tick, @tick_interval)
    end
  end

  # Mark staleness
  defp mark_stale(id) do
    GenServer.cast(via_tuple(id), :mark_stale)
  end
  defp setup_staleness_flagger(
         %{"type" => "filesystem",
           "watch_paths" => watch_paths_lua_array,
           "fs_event_handler" => fs_event_handler} =
           _staleness_flagger,
         initial_state
       ) do
  
    watch_paths = Melbyd.LuerlUtil.array_to_native_list(watch_paths_lua_array)
    Logger.info("Watching filesystem directory #{inspect(watch_paths)}")
    {:ok, watcher_pid} = FileSystem.start_link(dirs: watch_paths)
    FileSystem.subscribe(watcher_pid)
  
    # We need to save this info about fs, because we need to run the fs event
    # handler (we can do the full lookup using get_resources but this is slightly
    # cheaper).
    Map.put(initial_state, :fs_event_handler, fs_event_handler)
  end
  defp setup_staleness_flagger(
         %{"type" => "duration", "duration" => duration} = _staleness_flagger,
         initial_state
       ) do
    Logger.info("Setting up duration-based staleness flagger, with duration #{duration}")
    :timer.send_interval(:timer.seconds(duration_to_seconds(duration)), self(), :duration_event)
  
    Map.put(initial_state, :duration, duration)
  end
  
  defp duration_to_seconds(s) do
    case Elixir.Timex.Parse.Duration.Parsers.ISO8601Parser.parse(s) do
      {:ok, d} ->
        seconds = Timex.Duration.to_seconds(d, truncate: true)
  
        if seconds == 0 do
          Logger.warning("duration #{s} was parsed as 0 seconds; using 2 seconds as fallback")
          2
        else
          seconds
        end
  
      {:error, err} ->
        Logger.warning("failed to parse duration #{s}: #{inspect(err)}; using 2 seconds as fallback")
        2
    end
  end
  # Filesystem-based staleness
  
  # Mark the given path as stale, as well as all other SRS GenServers whose id's
  # are of the form "{resource_type, path}" where "path" is a parent path.
  defp mark_all_paths_stale_from({resource_type, path}) do
    get_all_parents(path)
    |> Enum.map(fn p -> mark_stale({resource_type, p}) end)
  end
  
  # Given "/a/b/c", return ["/", "/a", "/a/b", "/a/b/c"]
  defp get_all_parents(path) do
    parts = Path.split(path)
    parts_len = length(parts)
  
    1..parts_len
    |> Enum.map(&(Enum.take(parts, &1) |> Path.join()))
  end

  # Client interface
  def read(resource, resource_opts) do
    # At this point we have all the information we need in order to instantiate a
    # new SRS GenServer. We need to start it up (if necessary) and get information
    # out of it. This optional startup can be handled by the DynamicSupervisor,
    # which can do a call into gproc (process registry) to determine if the
    # GenServer of the type and options exists.
  
    resource_id =
      cond do
        resource_opts["fake"] ->
          resource["fake"]["resource_id_func"].([resource_opts]) |> Kernel.hd()
  
        resource["resource_id_command"] != nil ->
          run_resource_id_command(resource, resource_opts)
  
        resource["resource_id_func"] != nil ->
          resource["resource_id_func"].([resource_opts]) |> Kernel.hd()
  
        true ->
          ""
      end
  
    if resource_id == "" do
      Logger.warning(
        "resource_id cannot be empty: failed to generate srs_id for resource " <>
          "#{inspect({resource, resource_opts})} --- if this is a fake, then " <>
          "it means that your resource_id_func could be returning an empty string"
      )
  
      %MelbyDaemon.StandardResource{status: :STANDARD_RESOURCE_STATUS_NOT_APPLICABLE}
    else
      # Warn users about misbehaving resource_ids for non-fake resources.
      if String.starts_with?(resource_id, "fake->") && !resource_opts["fake"] do
        Logger.warning(
          "resource_id starts with 'fake->' but 'fake' key is not set in"
            <> "resource_opts: #{inspect({resource, resource_opts})}"
        )
  
        %MelbyDaemon.StandardResource{status: :STANDARD_RESOURCE_STATUS_NOT_APPLICABLE}
      else
        # Prepend "fake->" to the resource_id so that it is in a different
        # "namespace" and does not clash with real resource ids. It could be the
        # case that the real resource's id command or function would also output a
        # leading "fake->" string, but this is very unlikely.
        resource_id =
          if resource_opts["fake"] do
            "fake->#{resource_id}"
          else
            resource_id
          end
  
        # We need to encode the resource type as well into the id because it may
        # be the case that other resource types also end up generating the same
        # id, such as when both resource types depend on the same filesystem
        # path.
        srs_id = {resource["type"], resource_id}
  
        case :gproc.lookup_pids({:n, :l, {Melbyd.StandardResource, srs_id}}) do
          [pid] ->
            # This StandardResource with the given id already exists.
            Logger.info("Found existing pid for #{inspect(srs_id)}: #{inspect(pid)}")
  
            # If it's a fake resource, we manually mark it stale first, then force
            # a read (with a tick).
            if resource_opts["fake"] do
              GenServer.call(pid, :mark_stale)
              GenServer.call(pid, :tick)
            end
  
            GenServer.call(pid, :read)
  
          _ ->
            # Start the StandardResource with the given id. This is idempotent and
            # will not spawn a new GenServer if one already exists with the given
            # id.
            #
            # Because we wrap the start_watcher() call inside a Task, it also runs
            # asynchronously (so that we don't block until the startup is finished
            # before returning the "LOADING" status below).
            Task.Supervisor.start_child(Melbyd.TaskSupervisor, fn ->
              Melbyd.StandardResourceSupervisor.start_srs(srs_id, resource, resource_opts)
            end)
  
            # We started the watcher just above asynchronously. For now return a
            # blank struct with the "LOADING" status so that the caller can know
            # that the given repo is indeed a Git repo but that we just don't have
            # any data yet.
            %MelbyDaemon.StandardResource{status: :STANDARD_RESOURCE_STATUS_LOADING}
        end
      end
    end
  end
  
  # Return a resource_id by running the given command. Also return the appropriate
  # StandardResourceStatus atom.
  def run_resource_id_command(resource, resource_opts) do
    # When we call a luerl-decoded function, we have to pass in arguments as a
    # list, as in [resource_opts] below.
    [resource_id_command_luerl_table] = resource["resource_id_command"].([resource_opts])
    resource_id_command = Melbyd.LuerlUtil.table_to_native_map(resource_id_command_luerl_table)
    invocation = resource_id_command["invocation"] |> Melbyd.LuerlUtil.array_to_native_list()
    cmd_head = Kernel.hd(invocation)
    cmd_args = Enum.drop(invocation, 1)
    cd = resource_id_command["cd"]
  
    cmd_opts =
      if cd != nil do
        [cd: cd]
      else
        []
      end
  
    # If the resource id command requirse some additional processing (the command
    # itself does not return a unique, simple string), we can construct our final
    # format with the help of the parser.
    parser_func_name = resource_id_command["parser"]
    parser_func = resource["parser"][parser_func_name] || (&Function.identity/1)
  
    case System.cmd(cmd_head, cmd_args, cmd_opts) do
      {stdout, 0} ->
        # For example, "git rev-parse ..." can output a trailing newline, which we
        # need to remove.
        stdout_trimmed = String.trim_trailing(stdout)
        resource_id = stdout_trimmed
  
        if resource_id == "" do
          Logger.warning(
            "command returned successfully, but had no output: failed to " <>
              "generate srs_id for resource #{inspect({resource, resource_opts})}"
          )
        end
  
        # If we have an associated parser function, use it to help construct the
        # final ID format. Otherwise (or if it errors out due to an invalid
        # input), just use the output we got from above.
        parsed_resource_id =
          if parser_func != nil do
            parser_func.([stdout_trimmed]) |> Kernel.hd()
          end
  
        resource_id =
          if parsed_resource_id != nil && String.trim(parsed_resource_id) != "" do
            parsed_resource_id
          end
  
        resource_id
  
      {_stdout, error_code} ->
        Logger.warning(
          "resource_id_command failed with error code #{error_code}: failed to " <>
            "generate srs_id for resource #{inspect({resource, resource_opts})}"
        )
  
        ""
    end
  end
end

# Supervisor.
defmodule Melbyd.StandardResourceSupervisor do
  @moduledoc """
  StandardResource GenServers are created dynamically during runtime. This
  module supervises these servers so that they are restarted if they fail
  unexpectedly.
  """

  # This automatically defines child_spec/1
  use DynamicSupervisor

  require Logger

  def start_link(init_arg) do
    Logger.info("Starting SRS dynamic supervisor")
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_srs(srs_id, resource, resource_opts) do
    case start_child(srs_id, resource, resource_opts) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      unknown -> Logger.warning("start_srs failed: #{inspect(unknown)}")
    end
  end

  defp start_child(srs_id, resource, resource_opts) do
    ttl = Application.get_env(:melbyd, :melbyd_srs_ttl)

    # We pass in the srs_id ({resource_type, resource_id}) and ttl as an
    # argument to the start_link/1 function of Melbyd.StandardResource.
    #
    # IOW, start_child() invokes the start_link() function of
    # Melbyd.StandardResource.
    DynamicSupervisor.start_child(
      __MODULE__,
      {Melbyd.StandardResource, %{id: srs_id,
                                 resource: resource,
                                 resource_opts: resource_opts,
                                 ttl: ttl,
                                 notify_on_exit_pid: nil}}
    )
  end
end
