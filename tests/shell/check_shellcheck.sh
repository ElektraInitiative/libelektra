#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS SHELLCHECK TEST
echo

command -v shellcheck > /dev/null 2>&1 || {
	echo "shellcheck command needed for this test, aborting" >&2
	exit 0
}

cd "@CMAKE_SOURCE_DIR@" || exit

# shellcheck disable=SC2046
set $(
	. "scripts/dev/list-shell-scripts" && list_shell_scripts
)
printf 'Checking Scripts\n'
printf '————————————————\n\n'
for file; do printf '%s\n' "$file"; done
printf '\n'
shellcheck --severity=warning "$@"
ret=$?
test $ret -eq 0
exit_if_fail "shellcheck found an issue, please check."

end_script
