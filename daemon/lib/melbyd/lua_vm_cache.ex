defmodule Melbyd.LuaVmCache do
  @moduledoc """
  Cache of loaded (and validated) Lua VMs, keyed by the path of the Lua config.
  """

  use GenServer
  require Logger

  @doc """
  Return the cache entry for the Lua config at the given path, building it first
  if necessary. The entry is a map with the loaded Lua VM state (:vm), the
  validated view params types (:view_params_types), and the files that the config
  loaded (:files) along with their hashes (:hashes).
  """
  def get(lua_file) do
    lua_file = Path.expand(lua_file)

    case :persistent_term.get({__MODULE__, lua_file}, nil) do
      nil ->
        GenServer.call(__MODULE__, {:get, lua_file}, :infinity)

      entry ->
        if hashes(entry.files) != entry.hashes do
          GenServer.cast(__MODULE__, {:refresh, lua_file})
        end

        {:ok, entry}
    end
  end

  @doc """
  Return how many times we have built a VM for the given config. Useful for
  debugging (and testing) the cache.
  """
  def build_count(lua_file) do
    GenServer.call(__MODULE__, {:build_count, Path.expand(lua_file)})
  end

  # Hash the contents of each file. A missing (or unreadable) file gets the error
  # reason instead, so that deleting a file also counts as a change.
  defp hashes(files) do
    Enum.map(files, fn file ->
      case File.read(file) do
        {:ok, contents} -> :crypto.hash(:sha256, contents)
        {:error, reason} -> reason
      end
    end)
  end

  # How long to wait before retrying a failed build, even if none of the files
  # that we know about changed.
  @retry_after_ms 5_000

  def start_link(_init_arg) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    {:ok, %{building: %{}, failed: %{}, build_counts: %{}}}
  end

  @impl true
  def handle_call({:get, lua_file}, from, state) do
    case :persistent_term.get({__MODULE__, lua_file}, nil) do
      nil ->
        case recent_failure(state, lua_file) do
          nil -> {:noreply, start_build(state, lua_file, [from])}
          reason -> {:reply, {:error, reason}, state}
        end

      entry ->
        {:reply, {:ok, entry}, state}
    end
  end

  @impl true
  def handle_call({:build_count, lua_file}, _from, state) do
    {:reply, Map.get(state.build_counts, lua_file, 0), state}
  end

  @impl true
  def handle_cast({:refresh, lua_file}, state) do
    if recent_failure(state, lua_file) == nil do
      {:noreply, start_build(state, lua_file, [])}
    else
      {:noreply, state}
    end
  end

  # The build finished (whether it succeeded or not).
  @impl true
  def handle_info({ref, result}, state) when is_reference(ref) do
    Process.demonitor(ref, [:flush])
    {:noreply, finish_build(state, ref, result)}
  end

  # The build crashed.
  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, state) do
    {:noreply, finish_build(state, ref, {:error, "build crashed: #{inspect(reason)}", nil})}
  end

  # Start a build of the given config, unless one is running already. Either way,
  # add the waiters to the build.
  defp start_build(state, lua_file, waiters) do
    case state.building[lua_file] do
      nil ->
        Logger.info("building Lua VM for #{inspect(lua_file)}")
        old_entry = :persistent_term.get({__MODULE__, lua_file}, nil)
        known_files = if old_entry, do: old_entry.files, else: [lua_file]

        task =
          Task.Supervisor.async_nolink(Melbyd.TaskSupervisor, fn ->
            build(lua_file, known_files)
          end)

        put_in(state.building[lua_file], %{ref: task.ref, waiters: waiters})

      build ->
        put_in(state.building[lua_file].waiters, waiters ++ build.waiters)
    end
  end

  defp finish_build(state, ref, result) do
    case Enum.find(state.building, fn {_, build} -> build.ref == ref end) do
      nil ->
        state

      {lua_file, build} ->
        state = %{state | building: Map.delete(state.building, lua_file)}

        reply =
          case result do
            {:ok, entry} ->
              :persistent_term.put({__MODULE__, lua_file}, entry)
              {:ok, entry}

            {:error, reason, _known_files} ->
              Logger.warning("failed to build Lua VM for #{inspect(lua_file)}: #{reason}")
              {:error, reason}
          end

        Enum.each(build.waiters, &GenServer.reply(&1, reply))

        state = update_in(state.build_counts[lua_file], &((&1 || 0) + 1))

        case result do
          {:ok, _} ->
            %{state | failed: Map.delete(state.failed, lua_file)}

          {:error, reason, known_files} ->
            # If the build crashed, we don't know which files it read. Assume
            # the config itself.
            known_files = known_files || [lua_file]

            failure = %{
              files: known_files,
              hashes: hashes(known_files),
              reason: reason,
              at: System.monotonic_time(:millisecond)
            }

            put_in(state.failed[lua_file], failure)
        end
    end
  end

  # Return the reason for the last failed build of this config, if it failed
  # recently and none of the files it knew about have changed since.
  defp recent_failure(state, lua_file) do
    case state.failed[lua_file] do
      nil ->
        nil

      failure ->
        age = System.monotonic_time(:millisecond) - failure.at

        if age < @retry_after_ms and hashes(failure.files) == failure.hashes do
          failure.reason
        else
          nil
        end
    end
  end

  # How many times to try building a VM if its files keep changing underneath us.
  @build_tries 3

  # Build the cache entry for the given config. Return {:ok, entry} on success, or
  # {:error, reason, files} on failure, where files are the files that we know
  # the config loaded (if any).
  defp build(lua_file, known_files, tries \\ @build_tries) do
    hashes_before = hashes(known_files)

    with {:ok, view_params_types} <- Melbyd.LuaConfigValidation.validate(lua_file),
         {:ok, _, vm} <- Melbyd.LuaInterop.run_file(lua_file),
         {:ok, files} <- loaded_files(lua_file, vm) do
      entry = %{
        vm: vm,
        view_params_types: view_params_types,
        files: files,
        hashes: hashes(files)
      }

      cond do
        hashes(known_files) == hashes_before ->
          {:ok, entry}

        tries > 1 ->
          Logger.info("#{inspect(lua_file)} changed while building its Lua VM; retrying")
          build(lua_file, files, tries - 1)

        true ->
          {:error, "#{inspect(lua_file)} kept changing while building its Lua VM", files}
      end
    else
      {:error, reason} when is_binary(reason) -> {:error, reason, known_files}
      {:error, reason} -> {:error, inspect(reason), known_files}
      err -> {:error, inspect(err), known_files}
    end
  end

  @loaded_files_lua """
  local files = {}
  for name in pairs(package.loaded) do
    local file = package.searchpath(name, package.path)
    if file then
      files[#files + 1] = file
    end
  end
  return files
  """

  # Return the config itself and every file that it loaded with "require".
  defp loaded_files(lua_file, vm) do
    case Luerl.do_dec(vm, @loaded_files_lua) do
      {:ok, [files], _vm} ->
        required = files |> Melbyd.LuerlUtil.array_to_native_list() |> Enum.sort()
        {:ok, Enum.uniq([lua_file | required])}

      err ->
        {:error, "could not list the files loaded by #{inspect(lua_file)}: #{inspect(err)}"}
    end
  end
end
