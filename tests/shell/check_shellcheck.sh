#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS SHELLCHECK TEST
echo

command -v shellcheck > /dev/null 2>&1 || {
	echo "shellcheck command needed for this test, aborting" >&2
	exit 0
}

shellcheck_version=$(shellcheck --version | sed -n 's/version: //p')
shellcheck_required_version="0.7.1"

shellcheck_min_version=$(
	printf '%s\n%s\n' "$shellcheck_version" "$shellcheck_required_version" |
		sort -V |
		head -1
)

if [ ! "$shellcheck_min_version" = "$shellcheck_required_version" ]; then
	echo "shellcheck version ${shellcheck_required_version} or later required, aborting"
	exit 0
fi

cd "@CMAKE_SOURCE_DIR@" || exit

# shellcheck disable=SC2046
set $(
	{ . "scripts/dev/list-shell-scripts" && list_shell_scripts; } |
		grep -v -f "tests/shell/check_shellcheck_ignorelist.txt"
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
