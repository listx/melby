local KubeFake = {}

-- time_idx is a point in time. It captures the idea of running this function
-- multiple times. If we want to always return the same result, we can ignore
-- the time_idx. But if we want to return certain values in a particular order,
-- we can use this time_idx, where 0 means the first time this function is
-- called, 1 is the next time, and so on. The time_idx will always wrap back to
-- 0 after reaching FIXME (max time_idx value).
function get_pods_fake(context, namespace, time_idx)
  local input = ""
  local output = {}

  if context == "one" then
    if time_idx == 0 then
      input = [[
Pending
Pending
Pending
Pending
Pending
Running
Running
Unknown
]]
      output["pods"] = 8
      output["pods_in_phase_Pending"] = 5
      output["pods_in_phase_Running"] = 2
      output["pods_in_phase_Succeeded"] = 0
      output["pods_in_phase_Failed"] = 0
      output["pods_in_phase_Unknown"] = 1
    elseif time_idx == 1 then
      -- More things have started running.
      input = [[
Running
Running
Running
Running
Running
Running
Running
Unknown
]]
      output["pods"] = 8
      output["pods_in_phase_Pending"] = 0
      output["pods_in_phase_Running"] = 7
      output["pods_in_phase_Succeeded"] = 0
      output["pods_in_phase_Failed"] = 0
      output["pods_in_phase_Unknown"] = 1
    else
      -- Everything is running OK (including the previously unknown one)!
      input = [[
Running
Running
Running
Running
Running
Running
Running
Running
]]
      output["pods"] = 8
      output["pods_in_phase_Pending"] = 0
      output["pods_in_phase_Running"] = 8
      output["pods_in_phase_Succeeded"] = 0
      output["pods_in_phase_Failed"] = 0
      output["pods_in_phase_Unknown"] = 0
    end
  else
    if time_idx == 0 then
      input = [[
Pending
Failed
Failed
Succeeded
]]
      output["pods"] = 4
      output["pods_in_phase_Pending"] = 1
      output["pods_in_phase_Running"] = 0
      output["pods_in_phase_Succeeded"] = 1
      output["pods_in_phase_Failed"] = 2
      output["pods_in_phase_Unknown"] = 0
    elseif time_idx == 1 then
      -- The pending pod succeeded.
      input = [[
Succeeded
Failed
Failed
Succeeded
]]
      output["pods"] = 4
      output["pods_in_phase_Pending"] = 0
      output["pods_in_phase_Running"] = 0
      output["pods_in_phase_Succeeded"] = 2
      output["pods_in_phase_Failed"] = 2
      output["pods_in_phase_Unknown"] = 0
    else
      -- The older pods were deleted.
      input = [[
Succeeded
]]
      output["pods"] = 1
      output["pods_in_phase_Pending"] = 0
      output["pods_in_phase_Running"] = 0
      output["pods_in_phase_Succeeded"] = 1
      output["pods_in_phase_Failed"] = 0
      output["pods_in_phase_Unknown"] = 0
    end
  end

  return {input=input, output=output}
end

-- This "readers" function mirrors the regular "readers" function, except that
-- it requires an additional "time_idx".
function KubeFake.readers (resource_opts, time_idx)
  local context = resource_opts["context"]
  local namespace = resource_opts["namespace"]

  return {
    -- For the "get_pods" parser, use the get_pods_fake function.
    get_pods = get_pods_fake(context, namespace, time_idx)
  }

end

function KubeFake.resource_id_func (resource_opts)
  return resource_opts["context"] .. ":" .. resource_opts["namespace"]
end

return KubeFake
