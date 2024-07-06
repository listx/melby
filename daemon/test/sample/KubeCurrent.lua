local KubeCurrent = {}
KubeCurrent["type"] = "KubeCurrent"
KubeCurrent["history_size"] = 10
KubeCurrent["parser"] = {}
KubeCurrent["fake"] = require "KubeCurrentFake"

-- We need KUBECONFIG and HOME.
function KubeCurrent.read (self, env, var_names)
  local resource_opts = {}
  for _, var_name in ipairs(var_names) do
    resource_opts[var_name] = env[var_name]
  end

  return melbyd.read_standard_resource(self, resource_opts)
end

-- We treat the KUBECONFIG value (after we've done any expansions of variables
-- such as HOME) as the resource_id.
function KubeCurrent.resource_id_func (resource_opts)
  local s = resource_opts["KUBECONFIG"]
  -- If it's, empty, use "$HOME/.kube/config" as the default.
  if s == nil or s == "" then
    s = resource_opts["HOME"] .. "/.kube/config"
  end

  return s
end

function KubeCurrent.readers (resource_opts)
  return {{invocation={"kubectl", "config", "get-contexts"},
           parser="current_context_and_namespace",
           env={KUBECONFIG=resource_opts["KUBECONFIG"]}}}
end

function KubeCurrent.staleness_flaggers (resource_id)
  local kubeconfig = resource_id
  local filesystem = {}
  filesystem["type"] = "filesystem"
  filesystem["watch_paths"] = melbyd.split(kubeconfig, ":")

  -- Unilke for Git, we watch file paths directly (those specified in
  -- KUBECONFIG). We treat all events as "staleness flagging", and thus
  -- unconditionally return "true" here.
  filesystem["fs_event_handler"] = function (path, events)
    return true
  end

  -- We can technically have multiple staleness flaggers.
  return {filesystem}
end

-- Split by newline, then for each line: split by whitespace to get all words.
-- then check that the first word is a "*" and use the 2nd word for the context
-- and 5th word for the namespace (if empty, use 'default')
function KubeCurrent.parser.current_context_and_namespace (s)
  local ret = {}

  local words = {}
  local lines = melbyd.get_lines_trimmed_nonempty(s)
  for _, line in ipairs(lines) do

    -- NOTE: gmatch is not implemented in luerl; see
    -- https://github.com/rvirding/luerl/issues/150. So instead we have to call
    -- out to Elixir again.
    words = melbyd.split(line, " ")

    if (words[1] or "") == "*" then
      local context = words[2]
      local namespace = words[5]
      if context == nil or context == "" then
        break
      end
      -- Technically the `namespace` here will never be the empty string because
      -- we only matched (of at least 1 character) some number of non-whitespace
      -- characters, or we didn't and namespace (words[5]) is simply nil.
      if namespace == nil or namespace == "" then
        namespace = "default"
      end
      -- Happy path.
      ret["context"] = context
      ret["namespace"] = namespace
      return ret
    end
  end

  -- Write safe defaults to make comparisons less painful in the main Lua config.
  melbyd.log("KubeCurrent returned empty string; using empty strings as defaults")
  ret["context"] = ""
  ret["namespace"] = ""
  return ret
end

function KubeCurrent.notify (resource_id, old, new)
  local kubeconfig = resource_id
  local message = {}
  -- Generate a new message if we change the current context or namespace.
  local old_context = old.kvs.context
  local new_context = new.kvs.context

  local old_namespace = old.kvs.namespace
  local new_namespace = new.kvs.namespace

  if new_context ~= old_context then
    message["topic"]="srs_KubeCurrent"
    message["from"]=kubeconfig
    message["payload"]={level="info",
                        text="Context changed from '" ..
                          old_context .. "' to '" ..
                          new_context .. "'."}
    melbyd.broadcast("srs_KubeCurrent", message)
  end

  if new_namespace ~= old_namespace then
    message["topic"]="srs_KubeCurrent"
    message["from"]=kubeconfig
    message["payload"]={level="info",
                        text="Namespace changed from '" ..
                          old_namespace .. "' to '" ..
                          new_namespace .. "'."}
    melbyd.broadcast("srs_KubeCurrent", message)
  end

  return nil
end

-- Only keep messages if the KUBECONFIG of the message "from" matches the
-- KUBECONFIG env var used by the current shell.
function KubeCurrent.should_keep_message (message, env_vars)
  local shell_kubeconfig = env_vars["KUBECONFIG"] or ""
  local kubeconfig = message["from"]

  return shell_kubeconfig == kubeconfig
end

function KubeCurrent.message_to_string (message)
  local widgets = {}
  local kubeconfig = message["from"]
  table.insert(widgets, {str="  [" .. message["payload"]["time"] .. "]"})
  table.insert(widgets, {str="[" .. message["payload"]["level"] .. "]"})
  table.insert(widgets, {str="[ K8s ]", fg="lightskyblue", styles={"bold"}})
  table.insert(widgets, {str=kubeconfig, fg="yellow", styles={"bold"},
                         drop_delim_right=true})
  table.insert(widgets, {str=": " .. message["payload"]["text"]})

  return melbyd.render(widgets, {str=" "})
end

-- FIXME: Allow a "substrs" argument here to pull in the already-rendered
-- cluster health info, so that it is inside the closing brace at the end (also
-- adjust asterisk accordingly to be there if we're loading either the
-- KubeCurrent OR Kube standard resource statuses)
function KubeCurrent.view (standard_resource, health_resource_status, health_str)
  local widgets = {}
  local kubecurrent = standard_resource["kvs"]

  if standard_resource.status == "STANDARD_RESOURCE_STATUS_LOADING" and
    (next(kubecurrent) == nil) then
    table.insert(widgets, {str="[ K8s... ]"})
  elseif kubecurrent.context ~= "" and kubecurrent.namespace ~= "" then
    table.insert(widgets, {str="[", drop_delim_right=true})
    -- utf8.char(9096) is "⎈"
    table.insert(widgets, {str=utf8.char(9096), fg="lightskyblue",
                           styles={"bold"}})
    table.insert(widgets, {str=kubecurrent.context, fg="lightskyblue",
                           styles={"bold"}, drop_delim_right=true})
    table.insert(widgets, {str=":", drop_delim_right=true})
    table.insert(widgets, {str=kubecurrent.namespace, fg="lightsalmon"})
    table.insert(widgets, {str=health_str, drop_delim_right=true})
    -- Append asterisk if Melby is serving stale data.
    if standard_resource.status == "STANDARD_RESOURCE_STATUS_LOADING" or
      health_resource_status == "STANDARD_RESOURCE_STATUS_LOADING" then
      table.insert(widgets, {str="]*", drop_delim_left=true})
    else
      table.insert(widgets, {str="]", drop_delim_left=true})
    end
  end

  return widgets
end

return KubeCurrent
