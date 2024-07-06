defmodule Melbyd.GRPC do
  use GRPC.Endpoint

  intercept GRPC.Server.Interceptors.Logger, level: :info

  run Melbyd.View.Service
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
    Melbyd.View.generate(req)
  end
end
