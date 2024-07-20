#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

tangled_files()
{
    git -C "${SCRIPT_ROOT}" grep -P '^#\+begin_\w+ .+? :tangle' -- '*.org' \
        | sed 's/^.\+:tangle \(\S\+\).*$/\1/' \
        | sort
}

tracked_files()
{
    git -C "${SCRIPT_ROOT}" ls-tree -r HEAD \
        | awk '{print $2,$4}' \
        | grep '^blob' \
        | grep -v '\.org$' \
        | awk '{print $2}'
}

main()
{
    diff -u <(tangled_files) <(tracked_files)
}

main "$@"
