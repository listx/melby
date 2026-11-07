#!/usr/bin/env bash
set -euo pipefail

# Example output from "git diff-index --cached HEAD" (the "..." is not literally
# there and is used to truncate these outputs to reasonable width):
#
# :100644 100644 1c458f... 32db82... M      doc/melbyd-model.org
# :100644 000000 9c4640... 000000... D      melbyd/test/sample/.melby.lua
# :000000 100755 000000... bdfb82... A      melbyd/test/sample/staged_size.sh
#
# We use the "-z" flag, so that each of these lines becomes two NUL-terminated
# fields (the metadata, and the path). This way we don't have to worry about
# special characters in paths.

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

  # If HEAD is a conflicted jj commit, everything in its ".jjconflict-*"
  # folders looks like a staged deletion. jj does not use the staging area, so
  # there is nothing to count.
  if git cat-file -e HEAD:JJ-CONFLICT-README 2>/dev/null; then
      echo 0
      exit
  fi

  mapfile -d '' -t fields < <(git diff-index -z --cached HEAD)

  metas=()
  paths=()
  for ((i = 0; i + 1 < ${#fields[@]}; i += 2)); do
    metas+=("${fields[i]}")
    paths+=("${fields[i + 1]}")
  done

  if (( ${#paths[@]} == 0 )); then
      echo 0
      exit
  fi

  # Find the paths that are treated as binary in .gitattributes, because we
  # don't count them. The output of "git check-attr -z" is a list of
  # NUL-terminated (path, attribute, value) triples.
  declare -A binary=()
  mapfile -d '' -t attrs < <(printf '%s\0' "${paths[@]}" \
    | git check-attr -z --stdin diff)
  for ((i = 0; i + 2 < ${#attrs[@]}; i += 3)); do
    if [[ "${attrs[i + 2]}" == unset ]]; then
      binary["${attrs[i]}"]=1
    fi
  done

  # Look up the sizes of all old and new objects at once. Missing objects (such
  # as the all-zeros object name for the "old" side of an addition) are just
  # missing from the sizes map, and count as 0.
  declare -A sizes=()
  while read -r name size; do
    if [[ "${size}" != missing ]]; then
      sizes["${name}"]="${size}"
    fi
  done < <(for meta in "${metas[@]}"; do
             read -r _ _ old new _ <<< "${meta}"
             echo "${old}"
             echo "${new}"
           done | sort -u | git cat-file --batch-check='%(objectname) %(objectsize)')

  total=0
  for ((i = 0; i < ${#paths[@]}; i++)); do
    path="${paths[i]}"
    if [[ -n "${binary[${path}]:-}" ]]; then
      continue
    fi

    read -r _ _ old_name new_name mode <<< "${metas[i]}"
    old="${sizes[${old_name}]:-0}"
    new="${sizes[${new_name}]:-0}"
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
