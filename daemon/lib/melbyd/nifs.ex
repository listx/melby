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
