#!/bin/sh

set -eu

if ! command -v git > /dev/null; then
	echo "git must be installed" >&2
	exit 1
fi

cd "$(git rev-parse --show-toplevel)" || exit 1

print_errors() {
	MSG="$1"
	ERRORS="$2"

	if [ -n "${GITHUB_ACTION-}" ]; then
		echo "$ERRORS"
	else
		echo "$MSG"
		echo "$ERRORS" | sed -e 's/^/    /g'
		echo
	fi
}

HAS_ERROR=0

CHECKS="${*:-dotslash dotdot internal}"

for check in $CHECKS; do
	case "$check" in
	"dotslash")
		# .../highlevel.*.mustache - contains mustache placeholders in includes
		# testmod_ - tests
		# qt-gui/unittest - tests
		QUOTE_NOT_DOT_SLASH=$(git grep -on --untracked -E -e '^\s*#include\s+".+"' --and --not -e '^\s*#include\s+"\./.*"' -- 'src' \
			':^src/tools/kdb/gen/templates/highlevel.*.mustache' \
			':^src/plugins/*/testmod_*' \
			':^src/tools/qt-gui/unittest' ||
			true)

		if [ -n "$QUOTE_NOT_DOT_SLASH" ]; then
			HAS_ERROR=1
			# shellcheck disable=SC2016
			print_errors 'Found `#include "..."` whose path does not start with ./:' "$QUOTE_NOT_DOT_SLASH"
		fi
		;;

	"dotdot")
		QUOTE_WITH_DOT_DOT=$(git grep -n --untracked -E -e '^\s*#include\s+["<].*/\.\./.*[">]' -- 'src' || true)

		if [ -n "$QUOTE_WITH_DOT_DOT" ]; then
			HAS_ERROR=1
			# shellcheck disable=SC2016
			print_errors 'Found `#include` containing /../:' "$QUOTE_WITH_DOT_DOT"
		fi
		;;

	"internal")
		INTERNAL_IN_PUBLIC=$(git grep -on --untracked -E -e '^\s*#include\s+["<]internal/.*[">]' -- 'src/include/elektra' || true)

		if [ -n "$INTERNAL_IN_PUBLIC" ]; then
			HAS_ERROR=1
			# shellcheck disable=SC2016
			print_errors 'Found #include of internal header in installed headers:' "$INTERNAL_IN_PUBLIC"
		fi
		;;
	esac
done

exit "$HAS_ERROR"
