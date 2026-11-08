defmodule Melbyd.GRPC do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger, level: :info

  run Melbyd.View.Service
  run Melbyd.Version.Service
end

defmodule Melbyd.View.Service do
  use GRPC.Server, service: MelbyClient.View.Service, compressors: [GRPC.Compressor.Gzip]

  @moduledoc """
  Legacy.
  """

  @doc """
  Retrieves the prompt.
  """
  @spec get_view(MelbyClient.ViewRequest.t(), GRPC.Server.Stream.t()) ::
          MelbyClient.ViewResponse.t()
  def get_view(req, _stream) do
    Melbyd.View.generate_within(req)
  end
end

defmodule Melbyd.Version.Service do
  use GRPC.Server, service: MelbyClient.Version.Service, compressors: [GRPC.Compressor.Gzip]

  @moduledoc """
  Tells melbyc which build of melbyd is running.
  """

  @spec get_version(MelbyClient.VersionRequest.t(), GRPC.Server.Stream.t()) ::
          MelbyClient.VersionResponse.t()
  def get_version(_req, _stream) do
    %MelbyClient.VersionResponse{
      version: to_string(Application.spec(:melbyd, :vsn)),
      root_dir: to_string(:code.root_dir())
    }
  end
end
