import Config

config :melbyd,
  env: config_env(),
  # MELBYD_SRS_TTL: Number of ticks (tick about 1 second) to go through (with 0
  # client usage) before exiting. This is a simple way to garbage-collect unused
  # SRS GenServers to prevent unbounded growth.
  melbyd_srs_ttl: System.get_env("MELBYD_SRS_TTL") || 3600 * 4,
  melbyd_sps_ttl: System.get_env("MELBYD_SLG_TTL") || 3600 * 4,
  melbyd_port:
    System.get_env("MELBYD_PORT") ||
      (if config_env() == :prod do
         50051
       else
         50052
       end)

# Avoid
#
#   .../latest_remote_poll.txt": read-only file system
#
# error with tzdata. Basically tzdata tries to self-update constantly, and it
# must do this in a writable directory. Currently this defaults to the Nix store
# for melbyd which is read-only, so we use the ~/.melby/tzdata directory instead.
# The ~/.melby directory is created on application startup.
config :tzdata, :data_dir, Path.expand("~/.melby/tzdata")
