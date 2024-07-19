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
       end),
  melbyr_port:
    System.get_env("MELBYR_PORT") ||
      (if config_env() == :prod do
         50055
       else
         50056
       end),
  # Users can set this variable to make melbyd use it directly (useful for
  # testing out development, non-packaged versions of melbyr). In production we
  # would simply expect to run the first "melbyr" binary found on the PATH.
  melbyr_path: System.get_env("MELBYR_PATH") || "melbyr"

# Avoid
#
#   .../latest_remote_poll.txt": read-only file system
#
# error with tzdata. Basically tzdata tries to self-update constantly, and it
# must do this in a writable directory. Currently this defaults to the Nix store
# for melbyd which is read-only, so we use the ~/.melby/tzdata directory instead.
# The ~/.melby directory is created on application startup.
config :tzdata, :data_dir, Path.expand("~/.melby/tzdata")
