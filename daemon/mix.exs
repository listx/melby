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
