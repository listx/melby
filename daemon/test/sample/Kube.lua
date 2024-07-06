local Kube = {}
Kube["type"] = "Kube"
Kube["history_size"] = 10
Kube["parser"] = {}
Kube["fake"] = require "KubeFake"

function Kube.read (self, context, namespace, melby_dir)
  local resource_opts = {}
  resource_opts["context"] = context
  resource_opts["namespace"] = namespace or "default"
  resource_opts["MELBY_DIR"] = melby_dir

  return melbyd.read_standard_resource(self, resource_opts)
end

function Kube.resource_id_func (resource_opts)
  return resource_opts["context"] .. ":" .. resource_opts["namespace"]
end

function Kube.readers (resource_opts)
  local context = resource_opts["context"]
  local namespace = resource_opts["namespace"]

  local readers = {
    {invocation={"kubectl", "get", "pods", "-o",
                 "go-template-file=" ..
                   resource_opts["MELBY_DIR"] ..
                   "/get_pods.gotemplate",
                 "--context=" .. context,
                 "--namespace=" .. namespace},
     parser="get_pods"},
  }
  return readers
end

-- We use a time-based staleness flagger which marks the model as stale every 2
-- seconds, just like the default setting of the `watch` UNIX command.
function Kube.staleness_flaggers (repo_root)
  local duration = {}
  duration["type"] = "duration"
  -- Mark as stale every 2 seconds (ISO 8601 format).
  duration["duration"] = "PT2S"

  return {duration}
end

-- This is from the go template.
function Kube.parser.get_pods (s)
  local ret = {}
  -- Example input strings (format is "PHASE"):
  --    Running
  --    Pending

  local phases = melbyd.get_lines_trimmed_nonempty(s)

  -- How many pods are listed?
  ret["pods"] = #phases

  local count = 0

  -- Construct mapping of pod phases and the number of pods in that phase.
  -- See https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle/#pod-phase.
  --
  -- The 5 possible phase values are: Pending, Running, Succeeded, Failed, Unknown.
  ret["pods_in_phase_Pending"] = 0
  ret["pods_in_phase_Running"] = 0
  ret["pods_in_phase_Succeeded"] = 0
  ret["pods_in_phase_Failed"] = 0
  ret["pods_in_phase_Unknown"] = 0
  for _, phase in ipairs(phases) do
    -- The "or 0" here only takes effect if we see an unknown phase. Basically
    -- it should never happen.
    count = ret["pods_in_phase_" .. phase] or 0
    ret["pods_in_phase_" .. phase] = count + 1
  end

  -- FIXME: Move this to the View section, because we shouldn't store redundant
  -- data.
  --
  -- Of the listed pods, how many are in a healthy "Running" phase? This is
  -- useful for service clusters where the pods are long-lived services that are
  -- expected to always be in a "Running" state (being available).
  --
  -- string.format("%.1f%%", p*100) -> "50.0%" for 1/2
-- if #phases == 0 then
--   ret["pods_percent_in_phase_Pending"]   = 0.0
--   ret["pods_percent_in_phase_Running"]   = 0.0
--   ret["pods_percent_in_phase_Succeeded"] = 0.0
--   ret["pods_percent_in_phase_Failed"]    = 0.0
--   ret["pods_percent_in_phase_Unknown"]   = 0.0
-- else
--   ret["pods_percent_in_phase_Pending"]   = ret["pods_in_phase_Pending"]   / #phases
--   ret["pods_percent_in_phase_Running"]   = ret["pods_in_phase_Running"]   / #phases
--   ret["pods_percent_in_phase_Succeeded"] = ret["pods_in_phase_Succeeded"] / #phases
--   ret["pods_percent_in_phase_Failed"]    = ret["pods_in_phase_Failed"]    / #phases
--   ret["pods_percent_in_phase_Unknown"]   = ret["pods_in_phase_Unknown"]   / #phases
-- end

  return ret
end

function Kube.notify (resource_id, old, new)
  local context_and_namespace = resource_id
  local message = {}
  -- Show a diff of all phase buckets. Also output a number of all running pods
  -- (as a basic sanity check that N pods are running for some service cluster).
  --
  -- Something like:
  --
  --    Pods: Total 6 (-3), Pending 4 (+1), Running 2 (-4), Succeeded 0, Failed 0, Unknown 0
  --
  -- Where we display the totals and the difference (vs old state in totals but
  -- also as a percentage) in parentheses.

  local diff_found = false
  local delta = {}
  local delta_with_sign = function (n)
    if n < 0 then
      return melbyd.render(
        {{str=tostring(n), fg="red", styles={"bold"}}},
        {str=" "})
    end

    return melbyd.render(
      {{str="+" .. n, fg="lime", styles={"bold"}}},
      {str=" "})
  end

  local keys = {
    "pods",
    "pods_in_phase_Pending",
    "pods_in_phase_Running",
    "pods_in_phase_Succeeded",
    "pods_in_phase_Failed",
    "pods_in_phase_Unknown",
  }
  local key_display_name = {}
  key_display_name["pods"] = "Total"
  key_display_name["pods_in_phase_Pending"] = "Pending"
  key_display_name["pods_in_phase_Running"] = "Running"
  key_display_name["pods_in_phase_Succeeded"] = "Succeeded"
  key_display_name["pods_in_phase_Failed"] = "Failed"
  key_display_name["pods_in_phase_Unknown"] = "Unknown"

  for _, key in ipairs(keys) do
    local old_val = old.kvs[key]
    local new_val = new.kvs[key]
    delta[key] = new_val - old_val
    diff_found = diff_found or (old_val ~= new_val)
  end

  if diff_found then
    local text = "Pods:"
    for i, key in ipairs(keys) do
      local sep = ", "
      if i == 1 then
        sep = " "
      end
      text = text .. sep ..
        string.format("%s %d", key_display_name[key], new.kvs[key])
      if delta[key] ~= 0 then
        text = text .. string.format(" (%s)", delta_with_sign(delta[key]))
      end
    end
    message["topic"]="srs_Kube"
    message["from"]=context_and_namespace
    message["payload"]={level="info", text=text}
    melbyd.broadcast("srs_Kube", message)
  end

  return nil
end

-- This resource is generally meant for detecting cluster health (pod stats), so
-- it's conceivable that all shells would like to know about this. So we just
-- always return "true".
-- FIXME: Is there a better default for this?
function Kube.should_keep_message (message, env_vars)
  return true
end

function Kube.message_to_string (message, pwd_pretty_options)
  local widgets = {}
  local context_and_namespace_array = melbyd.split(message["from"], ":")
  local context = context_and_namespace_array[1]
  local namespace = context_and_namespace_array[2]
  table.insert(widgets, {str="  [" .. message["payload"]["time"] .. "]"})
  table.insert(widgets, {str="[" .. message["payload"]["level"] .. "]"})
  table.insert(widgets, {str="[ K8s (Cluster Health) ]", fg="lightskyblue",
                         styles={"bold"}})
  table.insert(widgets, {str=context, drop_delim_right=true, fg="lightskyblue",
                         styles={"bold"}})
  table.insert(widgets, {str=":", drop_delim_right=true})
  table.insert(widgets, {str=namespace, drop_delim_right=true, fg="lightsalmon"})
  table.insert(widgets, {str=": " .. message["payload"]["text"]})

  return melbyd.render(widgets, {str=" "})
end

function Kube.view (standard_resource)
  local widgets = {}
  local kube = standard_resource["kvs"]

  -- Display pod information in a compact way.
  --
  -- Example:
  --
  --    10T 1p 5r 1s 2f 0u
  --
  -- for 10 total pods, 1 pending, 5 running, 1 succeeded, 2 failed, 0 unknown.
  --
  -- FIXME: Display percentages instead of raw pod counts. We already get raw
  -- pod counts in the messaging part.
  if standard_resource.status == "STANDARD_RESOURCE_STATUS_LOADING" and
    (next(kube) == nil) then
    table.insert(widgets, {str="(pods...)"})
  elseif kube.pods ~= nil then
    table.insert(widgets, {str=kube.pods .. "T"})
    table.insert(widgets, {str=kube.pods_in_phase_Pending .. "p"})
    table.insert(widgets, {str=kube.pods_in_phase_Running .. "r"})
    table.insert(widgets, {str=kube.pods_in_phase_Succeeded .. "s"})
    table.insert(widgets, {str=kube.pods_in_phase_Failed .. "f"})
    table.insert(widgets, {str=kube.pods_in_phase_Unknown .. "u"})
    -- Unlike for Git, we don't append an asterisk if we are serving "stale"
    -- data because technically the data is always stale and it is constantly
    -- refreshed.
  end

  return widgets
end

return Kube
