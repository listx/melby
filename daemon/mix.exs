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

defmodule Melbyd.MixProject do
  use Mix.Project

  def project do
    [
      app: :melbyd,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [
        :file_system,
        :gproc,
        :logger,
      ],
      mod: {Melbyd.Application, []},
    ]
  end

  defp aliases do
    [
      test: ["cmd cd lib/melbyd/nifs && make test", "test"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:cachex, "~> 3.4"},
      {:file_system, "~> 1.0"},
      {:gproc, "~> 1.0.0"},
      {:grpc, "~> 0.7.0"},
      {:luerl, "~> 1.0"},
      {:muontrap, "~> 1.4.0"},
      {:phoenix_pubsub, "~> 2.0"},
      {:protobuf, "~> 0.10"},
      {:rustler, "~> 0.31.0"},
      {:temp, "~> 0.4"},
      {:timex, "~> 3.7"},
      {:tz, "~> 0.26.5"},
      {:wait_for_it, "~> 2.1"}
    ]
  end
end
