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
