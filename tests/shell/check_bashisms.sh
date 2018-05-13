@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS BASHISMS TEST
echo

command -v checkbashisms >/dev/null 2>&1 || { echo "checkbashisms command needed for this test, aborting" >&2; exit 0; }

cd "@CMAKE_SOURCE_DIR@"

# this way we also check subdirectories
# The script `check-env-dep` uses process substitution which is **not** a standard `sh` feature!
# See also: https://unix.stackexchange.com/questions/151925
scripts=$(find -E scripts/ -type f -not -regex
	  '.+(/docker/.+|check-env-dep|Jenkinsfile(.daily)?|kdb_zsh_completion|sed|\.(cmake|fish|in|md|txt))$' | xargs)
checkbashisms $scripts
ret=$?
# 2 means skipped file, e.g. README.md, that is fine
# only 1, 3 and 4 are actually bad
test $ret -eq 0 || test $ret -eq 2
exit_if_fail "Possible bashisms detected, please check."

end_script
