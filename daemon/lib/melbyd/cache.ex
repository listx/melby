defmodule Melbyd.Cache.PathShorten do
  @moduledoc """
  path-shorten Cache
  """
  import Cachex.Spec

  @cache_id :path_pretty_cache

  def child_spec(_init_arg) do
    %{
      id: @cache_id,
      type: :supervisor,
      start:
        {Cachex, :start_link,
         [
           @cache_id,
           [
             # Evict the least recently written entries beyond 256 entries.
             hooks: [hook(module: Cachex.Limit.Evented, args: {256, []})]
           ]
         ]}
    }
  end
end
