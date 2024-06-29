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

defmodule MelbyRenderer.RenderFormat do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :RENDER_FORMAT_UNSPECIFIED, 0
  field :RENDER_FORMAT_UNIX_TERMINAL, 1
end

defmodule MelbyRenderer.RenderColorDepth do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :RENDER_COLOR_DEPTH_UNSPECIFIED, 0
  field :RENDER_COLOR_DEPTH_256, 1
  field :RENDER_COLOR_DEPTH_24_BIT, 2
end

defmodule MelbyRenderer.TextStyle do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :TEXT_STYLE_UNSPECIFIED, 0
  field :TEXT_STYLE_BOLD, 1
  field :TEXT_STYLE_ITALIC, 2
  field :TEXT_STYLE_UNDERLINE, 3
  field :TEXT_STYLE_UNDERLINE_DOUBLE, 4
  field :TEXT_STYLE_BLINK, 5
  field :TEXT_STYLE_BLINK_RAPID, 6
end

defmodule MelbyRenderer.ParseStatus do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :PARSE_STATUS_UNSPECIFIED, 0
  field :PARSE_STATUS_ERROR, 1
  field :PARSE_STATUS_OK, 2
end

defmodule MelbyRenderer.ColorizedGitShaRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :sha, 1, type: :string
  field :sha_length, 2, type: :uint32, json_name: "shaLength"
  field :pad_left, 3, type: :uint32, json_name: "padLeft"
  field :pad_right, 4, type: :uint32, json_name: "padRight"
  field :render_options, 5, type: MelbyRenderer.RenderOptions, json_name: "renderOptions"
end

defmodule MelbyRenderer.ColorizedGitShaResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :sha_colorized, 1, type: :string, json_name: "shaColorized"
end

defmodule MelbyRenderer.RenderOptions do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :format, 1, type: MelbyRenderer.RenderFormat, enum: true
  field :color_depth, 2, type: MelbyRenderer.RenderColorDepth, json_name: "colorDepth", enum: true
end

defmodule MelbyRenderer.RenderWidgetsRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :widgets, 1, repeated: true, type: MelbyRenderer.Widget
  field :delimiter, 2, type: MelbyRenderer.Widget
  field :render_options, 3, type: MelbyRenderer.RenderOptions, json_name: "renderOptions"
end

defmodule MelbyRenderer.RenderWidgetsResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :widgets_rendered, 1, type: :string, json_name: "widgetsRendered"
end

defmodule MelbyRenderer.Widget do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :str, 1, type: :string
  field :prop, 2, type: MelbyRenderer.TextProperty
  field :drop_delim_left, 3, type: :bool, json_name: "dropDelimLeft"
  field :drop_delim_right, 4, type: :bool, json_name: "dropDelimRight"
end

defmodule MelbyRenderer.TextProperty do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :fg, 1, type: MelbyRenderer.Color
  field :bg, 2, type: MelbyRenderer.Color
  field :styles, 3, repeated: true, type: MelbyRenderer.TextStyle, enum: true
end

defmodule MelbyRenderer.Color do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  oneof :color_oneof, 0

  field :color_24_bit, 1, type: MelbyRenderer.Color24Bit, json_name: "color24Bit", oneof: 0
  field :color_256, 2, type: :uint32, json_name: "color256", oneof: 0
end

defmodule MelbyRenderer.Color24Bit do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :red, 1, type: :uint32
  field :green, 2, type: :uint32
  field :blue, 3, type: :uint32
end

defmodule MelbyRenderer.ParsePathAliasesRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :path_aliases_raw, 1, type: :string, json_name: "pathAliasesRaw"
end

defmodule MelbyRenderer.ParsePathAliasesResponse.PathAliasesEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule MelbyRenderer.ParsePathAliasesResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :status, 1, type: MelbyRenderer.ParseStatus, enum: true

  field :path_aliases, 2,
    repeated: true,
    type: MelbyRenderer.ParsePathAliasesResponse.PathAliasesEntry,
    json_name: "pathAliases",
    map: true

  field :error, 3, type: :string
end

defmodule MelbyRenderer.Renderer.Service do
  @moduledoc false
  use GRPC.Service, name: "melby_renderer.Renderer", protoc_gen_elixir_version: "0.11.0"

  rpc :GetColorizedGitSha,
      MelbyRenderer.ColorizedGitShaRequest,
      MelbyRenderer.ColorizedGitShaResponse

  rpc :RenderWidgets, MelbyRenderer.RenderWidgetsRequest, MelbyRenderer.RenderWidgetsResponse

  rpc :ParsePathAliases,
      MelbyRenderer.ParsePathAliasesRequest,
      MelbyRenderer.ParsePathAliasesResponse
end

defmodule MelbyRenderer.Renderer.Stub do
  @moduledoc false
  use GRPC.Stub, service: MelbyRenderer.Renderer.Service
end