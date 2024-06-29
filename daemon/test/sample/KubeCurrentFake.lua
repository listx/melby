-- Copyright 2024 Linus Arver
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

local KubeCurrentFake = {}

-- We really only use the time_idx because it's simpler here.
function current_context_and_namespace_fake(time_idx)
  local input = ""
  local output = {}

  if time_idx == 0 then
    -- Set to context "one".
    input = [[
CURRENT   NAME          CLUSTER          AUTHINFO              NAMESPACE
*         one           local            cluster-admin         foo
          two           two              two                   default
]]
    output["context"] = "one"
    output["namespace"] = "foo"
  elseif time_idx == 1 then
    -- Change to context "two".
    input = [[
CURRENT   NAME          CLUSTER          AUTHINFO              NAMESPACE
          one           local            cluster-admin         default
*         two           two              two                   bar
]]
    output["context"] = "two"
    output["namespace"] = "bar"
  else
    -- Change to namespace "default".
    input = [[
CURRENT   NAME          CLUSTER          AUTHINFO              NAMESPACE
          one           local            cluster-admin         default
*         two           two              two                   default
]]
    output["context"] = "two"
    output["namespace"] = "default"
  end

  return {input=input, output=output}
end

function KubeCurrentFake.readers (resource_opts, time_idx)
  -- FIXME: The reader is supposed to be auto-invoked whenever the file(s)
  -- pointed by KUBECONFIG env var from the client changes (filesystem watch).
  -- So we need to send fake filesystem events as part of the validation
  -- strategy. Sending a filesystem event is as simple as sending the
  -- correctly-formed tuple to the SRS process.
  return {
    current_context_and_namespace =
      current_context_and_namespace_fake(kubeconfig, time_idx)
  }

end

function KubeCurrentFake.resource_id_func (resource_opts)
  return resource_opts["KUBECONFIG"]
end

return KubeCurrentFake
