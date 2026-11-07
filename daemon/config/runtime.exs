import Config

# Read an integer from the given env var, or return the default if it isn't set.
# Env vars are always strings, so we have to parse them ourselves.
integer_env = fn name, default ->
  case System.get_env(name) do
    nil -> default
    val -> String.to_integer(val)
  end
end

config :melbyd,
  env: config_env(),
  # MELBYD_SRS_TTL: Number of ticks (tick about 1 second) to go through (with 0
  # client usage) before exiting. This is a simple way to garbage-collect unused
  # SRS GenServers to prevent unbounded growth.
  melbyd_srs_ttl: integer_env.("MELBYD_SRS_TTL", 3600 * 4),
  melbyd_sps_ttl: integer_env.("MELBYD_SLG_TTL", 3600 * 4),
  melbyd_port:
    integer_env.(
      "MELBYD_PORT",
      if config_env() == :prod do
        50051
      else
        50052
      end
    )

# Avoid
#
#   .../latest_remote_poll.txt": read-only file system
#
# error with tzdata. Basically tzdata tries to self-update constantly, and it
# must do this in a writable directory. Currently this defaults to the Nix store
# for melbyd which is read-only, so we use the ~/.melby/tzdata directory instead.
# The ~/.melby directory is created on application startup.
config :tzdata, :data_dir, Path.expand("~/.melby/tzdata")
