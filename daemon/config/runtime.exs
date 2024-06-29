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

import Config

config :melbyd,
  env: config_env(),
  # MELBYD_SRS_TTL: Number of ticks (tick about 1 second) to go through (with 0
  # client usage) before exiting. This is a simple way to garbage-collect unused
  # SRS GenServers to prevent unbounded growth.
  melbyd_srs_ttl: System.get_env("MELBYD_SRS_TTL") || 3600 * 4,
  melbyd_sps_ttl: System.get_env("MELBYD_SPS_TTL") || 3600 * 4,
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
