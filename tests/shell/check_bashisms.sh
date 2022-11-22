#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS BASHISMS TEST
echo

command -v checkbashisms > /dev/null 2>&1 || {
	echo "checkbashisms command needed for this test, aborting" >&2
	exit 0
}

cd "@CMAKE_SOURCE_DIR@" || exit

# Use (non-emacs) extended regex for GNU find or BSD find
find -version > /dev/null 2>&1 > /dev/null && FIND='find scripts -regextype egrep' || FIND='find -E scripts'

# - The scripts `reformat-c`, `reformat-java` and `install-config-file` use `command -v`,
# which was optional in POSIX until issue 7. Since `which` is not part of POSIX
# at all `command -v` is probably the most portable solution to detect the
# location of a command.
set $(
	$FIND -type f -not \( \
		-path '*COPYING-CMAKE-SCRIPTS' -or \
		-path '*find-tools' -or \
		-path '*freebsd/provision.sh' -or \
		-path '*gitignore' -or \
		-path '*kdb_zsh_completion' -or \
		-path '*kdb-zsh-noglob' -or \
		-path '*install-config-file' -or \
		-path '*reformat-c' -or \
		-path '*reformat-java' -or \
		-path '*run_env' -or \
		-path '*sed' -or \
		-path '*update-infos-status' -or \
		-path '*zsh' -or \
		-regex '.+(Docker|Jenkins|Vagrant)file.*' -or \
		-regex '.+\.(cmake|fish|ini?|kdb|md|txt|rb)$' \
		\) | sort
)
exit_if_fail 'Unable to locate shell scripts via `find`'
printf 'Checking Scripts\n'
printf '————————————————\n\n'
for file; do printf '%s\n' "$file"; done
printf '\n'
checkbashisms "$@"
ret=$?
# 2 means skipped file, e.g. README.md, that is fine
# only 1, 3 and 4 are actually bad
test $ret -eq 0 || test $ret -eq 2
exit_if_fail "Possible bashisms detected, please check."

end_script
