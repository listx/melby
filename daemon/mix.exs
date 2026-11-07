defmodule Melbyd.MixProject do
  use Mix.Project

  def project do
    [
      app: :melbyd,
      version: "0.1.0",
      elixir: "~> 1.18",
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
      {:cachex, "~> 4.1"},
      {:file_system, "~> 1.1"},
      {:gproc, "~> 1.3"},
      {:grpc, "~> 1.0"},
      {:grpc_server, "~> 1.0"},
      {:luerl, "~> 1.5"},
      {:phoenix_pubsub, "~> 2.4"},
      {:protobuf, "~> 0.17.1"},
      {:rustler, "~> 0.38.0"},
      {:temp, "~> 0.4.9"},
      {:timex, "~> 4.0"},
      {:tz, "~> 0.28.4"},
      {:wait_for_it, "~> 2.6"}
    ]
  end
end
