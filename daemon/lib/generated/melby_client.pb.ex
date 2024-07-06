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