#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

export HOST="somehost"
export MELBY_ZSH_KEYMAP_INDICATOR="${1}"
export MELBY_LAST_CMD_EXIT_STATUS="${2}"
export MELBY_PATH_ALIASES_FILE="${SCRIPT_ROOT}/sample/path-aliases"
export MELBY_WANT_KUBECTL_ERROR
MELBY_DIR="${SCRIPT_ROOT}/sample"
LUA_PATH="${SCRIPT_ROOT}/sample/?.lua"
MELBYC_PATH="${SCRIPT_ROOT}/../../client/melbyc"

# 50052 is the port used for the development environment (run_dev).
MELBYD_PORT="${MELBYD_PORT:-50052}"

usage()
{
	>&2 cat <<-EOF
	usage:   $0 ZSH_KEYMAP_INDICATOR LAST_CMD_EXIT_STATUS SHELL_PID
	example: $0 I 128 \$\$
EOF
}

get_view()
{
	"${MELBYC_PATH}" \
        --melbyd-port "${MELBYD_PORT}" \
        view "${SCRIPT_ROOT}/sample/melby.lua" \
        --shell-pid "${3}"
}

main()
{
	if (( $# != 3 )); then
		usage
		return 1
	fi

	source <(get_view "$@")

	echo "GOT MELBY_PS1_LINE1: $MELBY_PS1_LINE1"
	echo "GOT MELBY_PS1_LINE2: $MELBY_PS1_LINE2"

	for m in "${MELBY_SHELL_MESSAGES[@]}"; do
		echo -e "GOT MESSAGE: ${m}"
	done
}

main "$@"
