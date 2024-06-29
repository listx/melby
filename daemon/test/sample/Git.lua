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

-- Git Lua module for melby.
local Git = {}
Git["type"] = "Git"
Git["history_size"] = 10
Git["parser"] = {}
Git["fake"] = require "GitFake"

-- Read the state of the world for this Git repo. Relies on Git.model.readers to
-- get the job done.
function Git.read (self, env, var_names)
  local resource_opts = {}
  for _, var_name in ipairs(var_names) do
    resource_opts[var_name] = env[var_name]
  end
  -- Gets the current state of the SRS GenServer's latest "head" of this
  -- resource. This is because this resource can have a number of states stored
  -- in its history (and here we retrieve the first one). Also note that this
  -- call is very cheap because sometimes we will just read the already-stored
  -- history item (that is, calling this function may or may not result in
  -- running the Git.model.readers() commands).
  return melbyd.read_standard_resource(self, resource_opts)
end

-- Used to generate a unique resource_id. E.g., for Git, this is the absolute
-- folder path of the git root folder. This is because for Git, clients can
-- legitimately ask questions about the Git repo as long as they are in a child
-- folder under the root. Other resources that are filesystem-based will likely
-- be the same (e.g., direnv).
function Git.resource_id_command (resource_opts)
  local pwd = resource_opts["PWD"]
  return {cd=pwd, invocation={"git", "rev-parse", "--show-toplevel"}}
end

function Git.readers (resource_opts)
  local pwd = resource_opts["PWD"]
  -- Each reader needs an associated parser. The parser function is just the
  -- string name of a Git.parser.* function name. So parser="diff" refers to
  -- Git.parser.diff().
  --
  -- FIXME: Move the invocation definition to a function, and put this function
  -- next to each parser function.
  local readers = {
    {cd=pwd, invocation={"git", "diff", "--shortstat"},
     parser="diff"},

    {cd=pwd, invocation={"git", "diff", "--shortstat", "--cached"},
     parser="diff_cached"},

    {cd=pwd, invocation={"git", "diff", "--numstat"},
     parser="diff_binary"},

    {cd=pwd, invocation={"git", "diff", "--numstat", "--cached"},
     parser="diff_cached_binary"},

    {cd=pwd, invocation={(resource_opts["MELBY_DIR"] or "") ..
         "/git_staged_bytes.sh", "quiet"},
     parser="staged_bytes"},

    {cd=pwd, invocation={"git", "log", "--no-walk",
                         "--format=head_author_date,%at%n" ..
                           "head_author_name,%an%n" ..
                           "head_committer_date,%ct%n" ..
                           "head_committer_name,%cn",
                         "HEAD"},
     parser="head_metadata"},

    {cd=pwd, invocation={"git", "ls-files", "--others", "--exclude-standard"},
     parser="untracked_files"},

    {cd=pwd, invocation={"git", "ls-files", "-v"},
     parser="assume_unchanged_files"},

    {cd=pwd, invocation={"git", "rev-list", "--left-right", "--count",
                         "HEAD...@{upstream}"},
     parser="count_head_vs_upstream"},

    {cd=pwd, invocation={"git", "rev-parse", "--abbrev-ref", "HEAD"},
     parser="head_branch"},

    {cd=pwd, invocation={"git", "rev-parse", "HEAD"},
     parser="head_sha"},

    {cd=pwd, invocation={"git", "rev-parse", "--is-bare-repository"},
     parser="bare"},

    {cd=pwd, invocation={"git", "stash", "list"},
     parser="stash_size"},

    {cd=pwd, invocation={"git", "submodule", "status"},
     parser="submodule_status"},
  }
  return readers
end

-- Staleness flaggers are objects that mark staleness. A stale resource is
-- refreshed (by re-reading the state of the world). Here we define a
-- filesystem-based staleness flagger which marks the model as stale, based on a
-- simple algorithm that ignores changes to Git index files (these files are
-- modified by the "git" binary).
function Git.staleness_flaggers (resource_id)
  local repo_root = resource_id
  local filesystem = {}
  filesystem["type"] = "filesystem"
  filesystem["watch_paths"] = {repo_root}

  -- Custom logic to check how to detect cache invalidation, and whether to also
  -- invalidate all parent SRS GenServers up along the path (if any).
  filesystem["fs_event_handler"] = function (path, events)
    -- For index files (".git/**/index.lock"), ignore all events except just
    -- ":modified".
    --
    -- When we do get a :modified event, this can happen
    -- if HEAD moves, which is important when we (for example) modify the
    -- HEAD of submodules.
    if string.find(path, "/.git/") and string.find(path, "/index.lock$") then
      -- FIXME note: Looks like
      -- https://github.com/rvirding/luerl/blob/5e61c1838d08430af67fb870995b05a41d64aeee/src/luerl.erl#L340
      -- converts an atom into a "binary" (string). This just drops the initial
      -- ":" and makes the rest a string as is.
      if #events == 1 and events[1] == "modified" then
        return true
      else
        -- For index files, ignore all other events such as [:modified, :closed]
        -- which can also happen with [:created] and [:deleted] whenever we
        -- invoke certain Git read-only operations (e.g., git status).
        return false
      end
    end
    -- NOTE: Conceivably, if we're using jj, we could also ignore any changes
    -- going into the "/.jj/" folder.
    return true
  end

  -- We can technically have multiple staleness flaggers, although there can be
  -- only 1 filesystem one (because all filesystem events are fed to the
  -- fs_event_handler above).
  return {filesystem}
end

function Git.parser.diff (s)
  local ret = {}
  -- Example input strings:
  -- " 1 file changed, 32 insertions(+), 2 deletions(-)"
  -- " 1 file changed, 35 insertions(+)"
  -- " 1 file changed, 8 deletions(-)"
  --
  -- Note that the "unstaged_files" here does include changed binary files.
  ret["unstaged_files"] = melbyd.get_int_group(s, "(\\d+) fil")
  ret["unstaged_insertions"] = melbyd.get_int_group(s, "(\\d+) ins")
  ret["unstaged_deletions"] = melbyd.get_int_group(s, "(\\d+) del")

  return ret
end

-- Same as Git.parser.diff, but with slightly different keys.
function Git.parser.diff_cached (s)
  local ret = {}
  ret["staged_files"] = melbyd.get_int_group(s, "(\\d+) fil")
  ret["staged_insertions"] = melbyd.get_int_group(s, "(\\d+) ins")
  ret["staged_deletions"] = melbyd.get_int_group(s, "(\\d+) del")

  return ret
end

-- Only count binary files that have changed. Simply count the number of lines
-- that start with "-".
function Git.parser.diff_binary (s)
  local ret = {}
  -- Example input:
  --    5   1   doc/build-literate.org
  --    -   -   doc/melby.html

  ret["unstaged_files_binary"] = melbyd.get_lines_matching_count(s, "-")

  return melbyd.cast_values(ret, {unstaged_files_binary="int"})
end

-- Same as Git.parser.diff, but with slightly different keys.
function Git.parser.diff_cached_binary (s)
  local ret = {}
  ret["staged_files_binary"] = melbyd.get_lines_matching_count(s, "-")

  return melbyd.cast_values(ret, {staged_files_binary="int"})
end

function Git.parser.untracked_files (s)
  local ret = {}

  local lines = melbyd.get_lines_trimmed_nonempty(s)
  ret["untracked_files"] = #lines

  return ret
end

function Git.parser.submodule_status (s)
  local ret = {}

  local lines = melbyd.get_lines_trimmed_nonempty(s)

  local uninitialzed = 0
  local out_of_sync = 0
  local merge_conflicts = 0

  for i=1,#lines do
    local c = string.sub(lines[i], 1, 1)
    if c == "-" then
      uninitialzed = uninitialzed + 1
    elseif c == "+" then
      out_of_sync = out_of_sync + 1
    elseif c == "U" then
      merge_conflicts = merge_conflicts + 1
    end
  end

  ret["submodule_uninitialized"] = uninitialzed
  ret["submodule_out_of_sync"] = out_of_sync
  ret["submodule_merge_conflicts"] = merge_conflicts

  return ret
end

function Git.parser.head_branch (s)
  local ret = {}

  -- This is "HEAD" if we are in detached HEAD state.
  ret["head_branch"] = melbyd.get_trimmed(s)

  return ret
end

function Git.parser.head_sha (s)
  local ret = {}

  ret["head_sha"] = melbyd.get_trimmed(s)

  return ret
end

function Git.parser.bare (s)
  local ret = {}

  ret["bare"] = melbyd.get_trimmed(s)

  return melbyd.cast_values(ret, {bare="bool"})
end

-- This function can fail even inside a normal Git repo (with "fatal: no
-- upstream configured for branch 'BRANCH_NAME").
function Git.parser.count_head_vs_upstream (s)
  if #s == 0 then
    return {count_upstream_to_head=0, count_head_to_upstream=0}
  end

  ret = melbyd.get_columnar_fields_zipped(s,
                                         {"count_upstream_to_head",
                                          "count_head_to_upstream"})

  return melbyd.cast_values(ret, {count_upstream_to_head="int",
                                 count_head_to_upstream="int"})
end

function Git.parser.stash_size (s)
  local ret = {}

  local lines = melbyd.get_lines_trimmed_nonempty(s)
  ret["stash_size"] = #lines

  return ret
end

function Git.parser.staged_bytes (s)
  local ret = {}

  if #s > 0 then
    ret["staged_bytes"] = tonumber(s)
  else
    -- This can happen if the command failed (generated an empty string).
    ret["staged_bytes"] = 0
  end

  return melbyd.cast_values(ret, {staged_bytes="int"})
end

function Git.parser.assume_unchanged_files (s)
  local ret = {}

  local lines = melbyd.get_lines_trimmed_nonempty(s)

  local count = 0

  for i=1,#lines do
    local ascii_num = string.byte(string.sub(lines[i], 1, 1))
    -- According to git-ls-files(1), the "-v" flag marks assume-unchanged files
    -- with a leading lowercase letter.
    if 0x61 <= ascii_num and ascii_num <= 0x7a then
      count = count + 1
    end
  end

  ret["assume_unchanged_files"] = count

  return ret
end

function Git.parser.head_metadata (s)
  local ret = {}
  ret = melbyd.get_kv_lines_as_map(s)
  return melbyd.cast_values(ret, {head_author_date="int",
                                 head_committer_date="int"})
end

function Git.notify (resource_id, old, new)
  local repo_root = resource_id
  local message = {}
  -- Look at overall state of the old resource versus the new resource. Generate
  -- messages for interesting state changes.
  --
  -- This is where we need to warn users about linting errors and such.
  --
  -- FIXME: If this is a "Rust" codebase, we could have a "Rust" model that
  -- performs linter and formatter checks.

  -- If we stage or unstage something, show tentative "commit size".
  local old_staged_bytes = old.kvs.staged_bytes
  local new_staged_bytes = new.kvs.staged_bytes
  melbyd.log("old_staged_bytes: " .. old_staged_bytes)
  melbyd.log("new_staged_bytes: " .. new_staged_bytes)
  if new_staged_bytes ~= old_staged_bytes then
    local size_rating = ""

    -- The sizes here are taken from the following geometric series:
    --
    -- ghci> map (\x -> round $ 2.718281828^x) [1..10]
    -- [3,7,20,55,148,403,1097,2981,8103,22026]

    if new_staged_bytes < 55 then
      size_rating = "XS"
    elseif new_staged_bytes < 148 then
      size_rating = "S"
    elseif new_staged_bytes < 403 then
      size_rating = "M"
    elseif new_staged_bytes < 1097 then
      size_rating = "L"
    elseif new_staged_bytes < 2981 then
      size_rating = "XL"
    else
      size_rating = "XXL"
    end
    message["topic"]="srs_Git"
    message["from"]=repo_root

    if new_staged_bytes == 0 then
      message["payload"]={level="info",
                          text="Staging area is now empty (nothing to commit)."}
    else
      message["payload"]={level="info",
                          text="Staged size is now " .. size_rating ..
                            " (" .. new_staged_bytes .. " bytes)."}
    end
    melbyd.broadcast("srs_Git", message)
  end

  return nil
end

-- For a ShellProcess that is receiving a message from us (Git resource), only
-- keep messages where the repo path is a prefix of the shell's pwd. To keep the
-- message, return true.
function Git.should_keep_message (message, env_vars)
  local pwd = env_vars["PWD"]
  local i
  local repo_path = message["from"]

  -- Discard message if the message was malformed and we could not determine the
  -- "repo path".
  if repo_path == nil then
    melbyd.log("repo_path is nil --- dropping")
    return false
  end

  if pwd == repo_path then
    return true
  end

  i, _ = string.find(pwd, repo_path)
  local repo_path_in_pwd_hierarchy = i == 1
  if repo_path_in_pwd_hierarchy then
    melbyd.log("repo_path " .. repo_path ..
               " is in pwd hierarchy (pwd is " .. pwd .. "); KEEPING")
  else
    melbyd.log("repo_path " .. repo_path ..
               " is NOT in pwd hierarchy (pwd is " .. pwd .. "); dropping")
  end
  return repo_path_in_pwd_hierarchy
end

function Git.message_to_string (message, pwd_pretty_options)
  -- Example:
  --
  --   [22:33:37] [info] [ Git ] ~/prog/melby: Staging area is now empty (nothing to commit).
  local widgets = {}
  local repo_path_pretty = melbyd.get_path_pretty(message["from"],
                                                  pwd_pretty_options)
  table.insert(widgets, {str="  [" .. message["payload"]["time"] .. "]"})
  table.insert(widgets, {str="[" .. message["payload"]["level"] .. "]"})
  table.insert(widgets, {str="[ Git ]", fg="lime", styles={"bold"}})
  table.insert(widgets, {str=repo_path_pretty, fg="yellow", styles={"bold"},
                         drop_delim_right=true})
  table.insert(widgets, {str=": " .. message["payload"]["text"]})

  return melbyd.render(widgets, {str=" "})
end

-- Function for rendering a Git model. Generates a list of widgets.
function Git.view (standard_resource)
  local widgets = {}
  local git = standard_resource["kvs"]

  -- If this is an initial inquiry into a brand new, uncached Git repo path
  -- (uncached as in Melby has not started tracking it yet), return a special "[
  -- Git... ]" string.
  --
  -- We do "next(table) == nil" to check if a table is truly empty, as per
  -- https://stackoverflow.com/a/1252776/437583.
  if standard_resource.status == "STANDARD_RESOURCE_STATUS_LOADING" and
    (next(git) == nil) then
    table.insert(widgets, {str="[ Git... ]"})
  elseif (not git.bare) and git.head_sha then
    table.insert(widgets, {str="[", drop_delim_right=true})
    colored_sha = melbyd.get_colorized_sha(git.head_sha, 8, 1, 1)
    table.insert(widgets, {str=colored_sha})

    if git.head_branch ~= "" then
      table.insert(widgets, {str=git.head_branch, fg="white", styles={"bold"}})
    end

    table.insert(widgets, {str=melbyd.get_relative_age_short(
                             git.head_committer_date),
                           fg="deepskyblue"})

    table.insert(widgets, {str=melbyd.get_truncated_personal_moniker(
                             git.head_author_name, 6),
                           fg="lime"})

    if git.count_upstream_to_head > 0 then
      -- Unfortunately, luerl does not appear to support "\u{...}" Lua 5.3
      -- syntax for encoding Unicode codepoints. So we have to use the
      -- utf8.char() function instead.
      --
      -- For completeness, "▲" is codepoint 0x25b2.
      table.insert(widgets, {str=utf8.char(0x25b2), drop_delim_right=true,
                             fg="lime"})
      table.insert(widgets, {str=tostring(git.count_upstream_to_head)})
    end

    if git.count_head_to_upstream > 0 then
      -- For completeness, "▼" is codepoint 0x25bc.
      table.insert(widgets, {str=utf8.char(0x25bc), drop_delim_right=true,
                             fg="red"})
      table.insert(widgets, {str=tostring(git.count_head_to_upstream)})
    end

    if (git.unstaged_files + git.unstaged_files_binary +
        git.unstaged_insertions + git.unstaged_deletions) > 0 then
      table.insert(widgets, {str="U", fg="cyan", styles={"bold"},
                             drop_delim_right=true})
      if git.unstaged_files > 0 then
        table.insert(widgets, {str=tostring(git.unstaged_files)})
      end
      if git.unstaged_files_binary > 0 then
        -- For completeness, "▝" is codepoint 0x259d.
        table.insert(widgets, {str=utf8.char(0x259d), fg="greenyellow",
                               styles={"bold"},
                               drop_delim_left=true, drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.unstaged_files_binary)})
      end
      if git.unstaged_insertions > 0 then
        table.insert(widgets, {str="+", fg="lime",
                               drop_delim_left=true, drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.unstaged_insertions),
                               fg="lime"})
      end
      if git.unstaged_deletions > 0 then
        table.insert(widgets, {str="-", fg="tomato", drop_delim_left=true,
                               drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.unstaged_deletions),
                               fg="tomato"})
      end
    end

    if (git.staged_files + git.staged_files_binary +
        git.staged_insertions + git.staged_deletions) > 0 then
      table.insert(widgets, {str="S", fg="pink", styles={"bold"},
                             drop_delim_right=true})
      if git.staged_files > 0 then
        table.insert(widgets, {str=tostring(git.staged_files)})
      end
      if git.staged_files_binary > 0 then
        table.insert(widgets, {str=utf8.char(0x259d), fg="greenyellow",
                               styles={"bold"},
                               drop_delim_left=true, drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.staged_files_binary)})
      end
      if git.staged_insertions > 0 then
        table.insert(widgets, {str="+", fg="lime", drop_delim_left=true,
                               drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.staged_insertions), fg="lime"})
      end
      if git.staged_deletions > 0 then
        table.insert(widgets, {str="-", fg="tomato", drop_delim_left=true,
                               drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.staged_deletions), fg="tomato"})
      end
    end

    if git.untracked_files > 0 then
      table.insert(widgets, {str="N", drop_delim_right=true, fg="gold",
                             styles={"bold"}})
      table.insert(widgets, {str=tostring(git.untracked_files)})
    end

    if git.stash_size > 0 then
      table.insert(widgets, {str="T", drop_delim_right=true, fg="purple",
                             styles={"bold"}})
      table.insert(widgets, {str=tostring(git.stash_size)})
    end

    if git.assume_unchanged_files > 0 then
      table.insert(widgets, {str="A", drop_delim_right=true, fg="red",
                             styles={"bold"}})
      table.insert(widgets, {str=tostring(git.assume_unchanged_files)})
    end

    if (git.submodule_uninitialized + git.submodule_out_of_sync +
        git.submodule_merge_conflicts) > 0 then
      table.insert(widgets, {str="M", drop_delim_right=true, fg="skyblue",
                             styles={"bold"}})
      table.insert(widgets, {str="{", drop_delim_right=true})
      if git.submodule_uninitialized > 0 then
        table.insert(widgets, {str="not_init=", drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.submodule_uninitialized)})
      end

      if git.submodule_out_of_sync > 0 then
        table.insert(widgets, {str="need_sync=", drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.submodule_out_of_sync)})
      end

      if git.submodule_merge_conflicts > 0 then
        table.insert(widgets, {str="conflict=", drop_delim_right=true})
        table.insert(widgets, {str=tostring(git.submodule_merge_conflicts)})
      end
      table.insert(widgets, {str="}", drop_delim_left=true})
    end

    -- Append asterisk if Melby is serving stale data.
    if standard_resource.status == "STANDARD_RESOURCE_STATUS_LOADING" then
      table.insert(widgets, {str="]*", drop_delim_left=true})
    else
      table.insert(widgets, {str="]", drop_delim_left=true})
    end
  end

  return widgets
end

return Git
