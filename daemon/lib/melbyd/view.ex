defmodule Melbyd.View do
  @moduledoc """
  Module to generate an arbitrary view (string).
  """

  require Logger

  # How long to wait for generate/1 in generate_within/2. Together with
  # @placeholder_ms, this leaves some time (out of the 500ms that we promise to
  # clients) for gRPC to send the response.
  @deadline_ms 450

  # How long to give Config.placeholder_view to run.
  @placeholder_ms 25

  @doc """
  Like generate/1, but give up waiting for it after deadline_ms milliseconds,
  and reply with a placeholder view (and the VIEW_STATUS_PENDING status)
  instead. Keep generating the view in the background even after we give up.
  """
  def generate_within(req, deadline_ms \\ @deadline_ms) do
    task = Task.Supervisor.async_nolink(Melbyd.TaskSupervisor, fn -> generate(req) end)

    # Task.ignore/1 makes the task keep running without us, but still gives us
    # the result if the task finished in the meantime.
    case Task.yield(task, deadline_ms) || Task.ignore(task) do
      {:ok, response} ->
        response

      {:exit, reason} ->
        %MelbyClient.ViewResponse{
          status: :VIEW_STATUS_ERROR,
          error: "failed to generate view: #{inspect(reason)}"
        }

      nil ->
        Logger.info("view for #{inspect(req.config_path)} took longer than #{deadline_ms}ms")

        %MelbyClient.ViewResponse{
          status: :VIEW_STATUS_PENDING,
          view: placeholder_view(req)
        }
    end
  end

  # Return the output of Config.placeholder_view, or the empty string if we
  # can't get it (quickly).
  defp placeholder_view(req) do
    task =
      Task.Supervisor.async_nolink(Melbyd.TaskSupervisor, fn ->
        with {:ok, %{vm: vm, view_params_types: view_params_types}} <-
               Melbyd.LuaVmCache.peek(req.config_path),
             {:ok, f} when f != nil <- Luerl.get_table_keys_dec(vm, ["Config", "placeholder_view"]) |> ok_value(),
             {:ok, env_vars_reduced} <-
               Melbyd.LuaConfigValidation.enforce_view_params_types(view_params_types, req.env_vars),
             {:ok, [view]} when is_binary(view) <-
               Melbyd.LuaInterop.call(vm, ["Config", "placeholder_view"], [env_vars_reduced, req.shell_pid]) do
          view
        else
          # There's no VM yet, or the config doesn't define a placeholder.
          :miss ->
            ""

          {:ok, nil} ->
            ""

          err ->
            Logger.warning("Config.placeholder_view failed: #{inspect(err)}")
            ""
        end
      end)

    case Task.yield(task, @placeholder_ms) || Task.shutdown(task, :brutal_kill) do
      {:ok, view} -> view
      _ -> ""
    end
  end

  defp ok_value({:ok, val, _st}), do: {:ok, val}
  defp ok_value(err), do: err

  def generate(req) do
    config_path = req.config_path
    env_vars = req.env_vars
    shell_pid = req.shell_pid

    Logger.info("interpreting config #{inspect(config_path)}")
    Logger.info("MELBY_DIR is #{inspect(env_vars["MELBY_DIR"])}")

    with :ok <- validate(config_path, env_vars, shell_pid),
         {:ok, %{vm: vm, view_params_types: view_params_types}} <-
           Melbyd.LuaVmCache.get(config_path),
         {:ok, env_vars_reduced} <- Melbyd.LuaConfigValidation.enforce_view_params_types(view_params_types, env_vars),
         {:ok, val} when is_list(val) and val != [] and is_binary(hd(val)) <-
           Melbyd.LuaInterop.call(vm, ["Config", "view"], [env_vars_reduced, shell_pid]) do
      [view] = val

      %MelbyClient.ViewResponse{
        status: :VIEW_STATUS_OK,
        view: view
      }
    else
      {:error, reason} ->
        %MelbyClient.ViewResponse{
          status: :VIEW_STATUS_ERROR,
          error: IO.inspect(reason)
        }

      unrecognized ->
        %MelbyClient.ViewResponse{
          status: :VIEW_STATUS_ERROR,
          error: "backend returned an unrecognized response: #{inspect(unrecognized)}"
        }
    end
  end

  # Perform some rudimentary validation.
  def validate(config_path, _env_vars, shell_pid) do
    cond do
      !File.exists?(config_path) ->
        {:error, "file #{config_path} does not exist"}

      # Luerl appends ".lua" to any file name that doesn't already end in it,
      # so it would fail to find (say) "melby.lua.orig".
      Path.extname(config_path) != ".lua" ->
        {:error, "file #{config_path} must end in .lua"}

      String.length(shell_pid) == 0 ->
        {:error, "shell_pid cannot be an empty string"}

      !String.match?(shell_pid, ~r/^[[:digit:]]+$/) ->
        {:error, "shell_pid '#{shell_pid}' has non-digit characters in it"}

      true ->
        :ok
    end
  end
end
