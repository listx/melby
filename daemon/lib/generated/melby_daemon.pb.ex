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

defmodule MelbyDaemon.StandardResourceStatus do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :STANDARD_RESOURCE_STATUS_UNSPECIFIED, 0
  field :STANDARD_RESOURCE_STATUS_NOT_APPLICABLE, 1
  field :STANDARD_RESOURCE_STATUS_LOADING, 2
  field :STANDARD_RESOURCE_STATUS_LOADED, 3
end

defmodule MelbyDaemon.StandardResource.KvsEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule MelbyDaemon.StandardResource do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :status, 1, type: MelbyDaemon.StandardResourceStatus, enum: true
  field :kvs, 2, repeated: true, type: MelbyDaemon.StandardResource.KvsEntry, map: true
end