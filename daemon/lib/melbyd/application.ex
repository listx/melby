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

defmodule Melbyd.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    # Make sure the ~/.melby/tzdata directory exists.
    File.mkdir_p!(Path.expand("~/.melby/tzdata"))

    children = [
      # (model)
      #
      # Equivalent to {Melbyd.Cache.PathShorten, []} --- and now this module must
      # define its own child_spec(arg) function.
      Melbyd.Cache.PathShorten,

      # Equivalent to {Task.Supervisor, [name: Melbyd.TaskSupervisor]}
      {Task.Supervisor, name: Melbyd.TaskSupervisor},
      Melbyd.StandardResourceSupervisor,
      Melbyd.ShellProcessSupervisor,

      # Lua config validation cache
      Melbyd.LuaConfigValidation,

      # PubSub messaging system. The "Melbyd.PubSub" here is just an atom, not an
      # actual Elixir Module. The sole purpose of it is to be a unique name,
      # from Phoenix.PubSub's perspective.
      {Phoenix.PubSub, name: Melbyd.PubSub},

      # gRPC service. (controller)
      {GRPC.Server.Supervisor, endpoint: Melbyd.GRPC, port: Application.get_env(:melbyd, :melbyd_port), start_server: true},

      # Haskell "melbyr" service. (view)
      {MuonTrap.Daemon,
       [Application.get_env(:melbyd, :melbyr_path),
        ["serve", "#{Application.get_env(:melbyd, :melbyr_port)}"],
       # FIXME: Make this more verbose output an environment variable option.
       #[stderr_to_stdout: true, log_output: :debug]]},
       [stderr_to_stdout: false]]},
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Melby.Supervisor]

    announce()

    Supervisor.start_link(children, opts)
  end

  # FIXME: Add ASCII-art here.
  defp announce() do
    Logger.info("Starting application in #{Application.fetch_env!(:melbyd, :env)} environment")
  end
end
