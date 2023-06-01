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

CHECKBASHISMS_VERSION=$(checkbashisms --version | head -n1 | rev | cut -d' ' -f1 | rev)
CHECKBASHISMS_VERSION_MAJOR=$(echo "$CHECKBASHISMS_VERSION" | cut -d. -f1)
CHECKBASHISMS_VERSION_MINOR=$(echo "$CHECKBASHISMS_VERSION" | cut -d. -f2)

if [ "$CHECKBASHISMS_VERSION_MAJOR" -lt 2 ] || [ "$CHECKBASHISMS_VERSION_MINOR" -lt 21 ]; then
	echo "checkbashisms version 2.21+ required"
	exit 0
fi

# Use (non-emacs) extended regex for GNU find or BSD find
find -version > /dev/null 2>&1 > /dev/null && FIND='find scripts -regextype egrep' || FIND='find -E scripts'

set $(
	$FIND -type f -not \( \
		-path '*COPYING-CMAKE-SCRIPTS' -or \
		-path '*find-tools' -or \
		-path '*freebsd/provision.sh' -or \
		-path '*gitignore' -or \
		-path '*kdb_zsh_completion' -or \
		-path '*kdb-zsh-noglob' -or \
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
