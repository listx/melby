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

defmodule Melbyd.View do
  @moduledoc """
  Module to generate an arbitrary view (string).
  """

  require Logger

  def generate(req) do
    config_path = req.config_path
    env_vars = req.env_vars
    shell_pid = req.shell_pid

    Logger.info("interpreting config #{inspect(config_path)}")
    Logger.info("MELBY_DIR is #{inspect(env_vars["MELBY_DIR"])}")

    with :ok <- validate(config_path, env_vars, shell_pid),
         {:ok, view_params_types} <- Melbyd.LuaConfigValidation.validate(config_path),
         {:ok, env_vars_reduced} <- Melbyd.LuaConfigValidation.enforce_view_params_types(view_params_types, env_vars),
         {:ok, val} when is_list(val) and val != [] and is_binary(hd(val)) <-
           Melbyd.LuaInterop.run(config_path, ["Config", "view"], [env_vars_reduced, shell_pid]) do
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
  # FIXME: Is this even worth it?
  def validate(config_path, _env_vars, shell_pid) do
    cond do
      !File.exists?(config_path) ->
        {:error, "file #{config_path} does not exist"}

      String.length(shell_pid) == 0 ->
        {:error, "shell_pid cannot be an empty string"}

      !String.match?(shell_pid, ~r/^[[:digit:]]+$/) ->
        {:error, "shell_pid '#{shell_pid}' has non-digit characters in it"}

      true ->
        :ok
    end
  end
end
