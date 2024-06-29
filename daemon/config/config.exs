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
