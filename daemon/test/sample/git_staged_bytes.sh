#!/usr/bin/env bash
#
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

set -euo pipefail

# Example output from "git diff-index HEAD --cached" (the "..." is not literally
# there and is used to truncate these outputs to reasonable width):
#
# :100644 100644 1c458f... 32db82... M      doc/melbyd-model.org
# :100644 000000 9c4640... 000000... D      melbyd/test/sample/.melby.lua
# :000000 100755 000000... bdfb82... A      melbyd/test/sample/staged_size.sh

__bytes()
{
  2>/dev/null git cat-file -s "${1}" || echo 0
}

__log()
{
  [[ "${log_level}" == "quiet" ]] && return
  echo >&2 "$@"
}

main()
{
  log_level="${1:-normal}"

  GIT_ROOT="$(git rev-parse --show-toplevel)"
  cd "${GIT_ROOT}"

  bare="$(git rev-parse --is-bare-repository)"
  if [[ "${bare}" == true ]]; then
      echo 0
      exit
  fi

  mapfile -t array < <(git diff-index HEAD --cached)

  total=0
  for line in "${array[@]}"; do
    parts=($line)
    path=${parts[5]}

    # If the path is not to be counted because it is treated as binary in
    # .gitattributes, skip it.
    if git check-attr diff -- "${path}" | grep -q "diff: unset"; then
      continue
    fi

    mode=${parts[4]}
    old=$(__bytes ${parts[2]})
    new=$(__bytes ${parts[3]})
    __log "path=${path}"
    __log "mode=${mode}"
    __log "old=${old}"
    __log "new=${new}"
    delta=0
    case "${mode}" in
      A) delta="${new}" ;;
      M) delta=$(( new - old )) ;;
      D) delta="${old}" ;;
    esac
    delta="${delta#-}"
    __log "delta=${delta}"
    __log
    total=$(( total + delta ))
  done

  echo "${total}"
}

main "$@"
