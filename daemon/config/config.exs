import Config

config :logger, :console,
  format: "$time [$level] $metadata $message\n",
  metadata: [:error_code, :file, :line, :mfa, :pid]

# Disable truncation of log messages.
config :logger,
  truncate: :infinity

config :elixir, :time_zone_database, Tz.TimeZoneDatabase

# This imports config/test.exs when we run unit tests. Otherwise that
# configuration (used during "mix test", called by "make test") is not used.
import_config "#{config_env()}.exs"
