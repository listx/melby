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

defmodule Melbyd.Path do
  @moduledoc """
  Caching wrapper around path shortening function.
  """

  require Logger

  def get_path_pretty(path, aliases, env_vars, shorten_threshold) do
    # The aliases and env_vars are lists. We leave them as such for acting as
    # keys to Cachex entries. But we do a conversion to a Map before calling the
    # Rust NIF, because the Rust functions expect a HashMap.
    {status, path_pretty} = Cachex.get(
      :path_pretty_cache,
      {path, aliases, env_vars})

    if status == :error || path_pretty == nil do
      # FIXME: Optionally colorize path depth. Maybe take in something like
      # keyword args...? Ideally user should be able to define a list of colors
      # to use for each directory depth (using modulo for cyclicness), as well
      # as the color of the slash and leading tilde (aliases).
      path_pretty =
        Melbyd.Nifs.path_shorten(
          path,
          aliases,
          env_vars,
          shorten_threshold
        )

      Cachex.put(:path_pretty_cache, {path, aliases, env_vars}, path_pretty)

      Logger.info(%{msg: "cache miss", path: path})
      path_pretty
    else
      path_pretty
    end
  end
end
