defmodule Melbyd.Cache.PathShorten do
  @moduledoc """
  path-shorten Cache
  """
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
             limit: 256
           ]
         ]}
    }
  end
end
