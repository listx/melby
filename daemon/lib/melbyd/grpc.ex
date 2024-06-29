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
