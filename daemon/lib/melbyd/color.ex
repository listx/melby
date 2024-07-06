defmodule Melbyd.Color do
  @moduledoc """
  Color parsing.
  """

  require Logger

  def parse(color_str) do
    {r, g, b, _a} = Melbyd.Nifs.parse_color(color_str)
    {r, g, b}
  end
end
