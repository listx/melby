defmodule MelbyDaemon.StandardResourceStatus do
  @moduledoc false

  use Protobuf,
    enum: true,
    full_name: "melby_daemon.StandardResourceStatus",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :STANDARD_RESOURCE_STATUS_UNSPECIFIED, 0
  field :STANDARD_RESOURCE_STATUS_NOT_APPLICABLE, 1
  field :STANDARD_RESOURCE_STATUS_LOADING, 2
  field :STANDARD_RESOURCE_STATUS_LOADED, 3
end

defmodule MelbyDaemon.StandardResource.KvsEntry do
  @moduledoc false

  use Protobuf,
    full_name: "melby_daemon.StandardResource.KvsEntry",
    map: true,
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule MelbyDaemon.StandardResource do
  @moduledoc false

  use Protobuf,
    full_name: "melby_daemon.StandardResource",
    protoc_gen_elixir_version: "0.17.1",
    syntax: :proto3

  field :status, 1, type: MelbyDaemon.StandardResourceStatus, enum: true
  field :kvs, 2, repeated: true, type: MelbyDaemon.StandardResource.KvsEntry, map: true
end
