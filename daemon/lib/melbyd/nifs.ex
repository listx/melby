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

defmodule Melbyd.Nifs do
  use Rustler,
    otp_app: :melbyd,
    crate: :melbyd_nifs,
    path: "lib/melbyd/nifs",
    skip_compilation?: true

  # When your NIFs are loaded, they will override these functions.
  def path_shorten(_path, _aliases, _env_vars, _shorten_threshold),
    do: :erlang.nif_error(:nif_not_loaded)

  def parse_color(_color_str), do: :erlang.nif_error(:nif_not_loaded)
end
