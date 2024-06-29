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

defmodule MelbyClient.ViewStatus do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :VIEW_STATUS_UNSPECIFIED, 0
  field :VIEW_STATUS_ERROR, 1
  field :VIEW_STATUS_OK, 2
end

defmodule MelbyClient.ViewRequest.EnvVarsEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule MelbyClient.ViewRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :config_path, 1, type: :string, json_name: "configPath"
  field :config, 2, type: :string

  field :env_vars, 3,
    repeated: true,
    type: MelbyClient.ViewRequest.EnvVarsEntry,
    json_name: "envVars",
    map: true

  field :shell_pid, 4, type: :string, json_name: "shellPid"
end

defmodule MelbyClient.ViewResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.11.0"

  field :status, 1, type: MelbyClient.ViewStatus, enum: true
  field :view, 2, type: :string
  field :error, 3, type: :string
end

defmodule MelbyClient.View.Service do
  @moduledoc false
  use GRPC.Service, name: "melby_client.View", protoc_gen_elixir_version: "0.11.0"

  rpc :GetView, MelbyClient.ViewRequest, MelbyClient.ViewResponse
end

defmodule MelbyClient.View.Stub do
  @moduledoc false
  use GRPC.Stub, service: MelbyClient.View.Service
end