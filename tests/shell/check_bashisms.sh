@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS BASHISMS TEST
echo

command -v checkbashisms > /dev/null 2>&1 || {
	echo "checkbashisms command needed for this test, aborting" >&2
	exit 0
}

cd "@CMAKE_SOURCE_DIR@"

# Use (non-emacs) extended regex for GNU find or BSD find
find -version > /dev/null 2>&1 > /dev/null && FIND='find scripts -regextype egrep' || FIND='find -E scripts'

# - The script `reformat-c` uses `command -v`, which is optional in POSIX. However, since `which` is not part of POSIX at all
#   `command -v` is probably the most portable solution to detect the location of a command.
#   See also: https://stackoverflow.com/questions/592620
scripts=$(
	$FIND -type f -not \( \
		-path '*COPYING-CMAKE-SCRIPTS' -or \
		-path '*find-tools' -or \
		-path '*freebsd/provision.sh' -or \
		-path '*gitignore' -or \
		-path '*kdb_zsh_completion' -or \
		-path '*kdb-zsh-noglob' -or \
		-path '*reformat-c' -or \
		-path '*run_env' -or \
		-path '*sed' -or \
		-path '*update-infos-status' -or \
		-path '*zsh' -or \
		-regex '.+(Docker|Jenkins|Vagrant)file.*' -or \
		-regex '.+\.(cmake|fish|ini?|kdb|md|txt|rb)$' \
		\) | xargs
)
exit_if_fail 'Unable to locate shell scripts via `find`'
checkbashisms $scripts
ret=$?
# 2 means skipped file, e.g. README.md, that is fine
# only 1, 3 and 4 are actually bad
test $ret -eq 0 || test $ret -eq 2
exit_if_fail "Possible bashisms detected, please check."

end_script
