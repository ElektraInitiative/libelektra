@INCLUDE_COMMON@

echo
echo ELEKTRA SCRIPTS BASHISMS TEST
echo

command -v checkbashisms >/dev/null 2>&1 || { echo "checkbashisms command needed for this test, aborting" >&2; exit 0; }

cd "@CMAKE_SOURCE_DIR@"

# this way we also check subdirectories
scripts=$(find scripts/ -type f | xargs)
checkbashisms $scripts
ret=$?
# 2 means skipped file, e.g. README.md, that is fine
# only 1, 3 and 4 are actually bad
test $ret -eq 0 || test $ret -eq 2
exit_if_fail "Possible bashisms detected, please check."

end_script
