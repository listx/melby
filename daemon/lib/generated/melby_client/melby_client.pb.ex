defmodule MelbyClient.ViewStatus do
  @moduledoc false

  use Protobuf,
    enum: true,
    full_name: "melby_client.ViewStatus",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :VIEW_STATUS_UNSPECIFIED, 0
  field :VIEW_STATUS_ERROR, 1
  field :VIEW_STATUS_OK, 2
  field :VIEW_STATUS_PENDING, 3
end

defmodule MelbyClient.ViewRequest.EnvVarsEntry do
  @moduledoc false

  use Protobuf,
    full_name: "melby_client.ViewRequest.EnvVarsEntry",
    map: true,
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule MelbyClient.ViewRequest do
  @moduledoc false

  use Protobuf,
    full_name: "melby_client.ViewRequest",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

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

  use Protobuf,
    full_name: "melby_client.ViewResponse",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :status, 1, type: MelbyClient.ViewStatus, enum: true
  field :view, 2, type: :string
  field :error, 3, type: :string
end

defmodule MelbyClient.VersionRequest do
  @moduledoc false

  use Protobuf,
    full_name: "melby_client.VersionRequest",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3
end

defmodule MelbyClient.VersionResponse do
  @moduledoc false

  use Protobuf,
    full_name: "melby_client.VersionResponse",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :version, 1, type: :string
  field :root_dir, 2, type: :string, json_name: "rootDir"
  field :git_rev, 3, type: :string, json_name: "gitRev"
end

defmodule MelbyClient.View.Service do
  @moduledoc false

  use GRPC.Service, name: "melby_client.View", protoc_gen_elixir_version: "0.17.1"

  rpc :GetView, MelbyClient.ViewRequest, MelbyClient.ViewResponse
end

defmodule MelbyClient.View.Stub do
  @moduledoc false

  use GRPC.Stub, service: MelbyClient.View.Service
end

defmodule MelbyClient.Version.Service do
  @moduledoc false

  use GRPC.Service, name: "melby_client.Version", protoc_gen_elixir_version: "0.17.1"

  rpc :GetVersion, MelbyClient.VersionRequest, MelbyClient.VersionResponse
end

defmodule MelbyClient.Version.Stub do
  @moduledoc false

  use GRPC.Stub, service: MelbyClient.Version.Service
end
