defmodule Melbyd.Nifs do
  use Rustler,
    otp_app: :melbyd,
    crate: :melbyd_nifs,
    path: "lib/melbyd/nifs",
    skip_compilation?: true,
    # We build the NIF library ourselves (see the Makefile and
    # melby-daemon.nix), with Cargo's default "lib" prefix.
    load_from: {:melbyd, "priv/native/libmelbyd_nifs"}

  # When your NIFs are loaded, they will override these functions.
  def path_shorten(_path, _aliases, _env_vars, _shorten_threshold),
    do: :erlang.nif_error(:nif_not_loaded)

  def parse_color(_color_str), do: :erlang.nif_error(:nif_not_loaded)

  def render_widgets(_widgets, _delimiter, _format), do: :erlang.nif_error(:nif_not_loaded)

  def colorize_git_sha(_sha, _sha_length, _pad_left, _pad_right),
    do: :erlang.nif_error(:nif_not_loaded)

  def parse_path_aliases(_path_aliases_raw), do: :erlang.nif_error(:nif_not_loaded)
end
