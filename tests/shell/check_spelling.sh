@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK SPELLING
echo

cd "@CMAKE_SOURCE_DIR@"

if ! git diff --exit-code; then
	printf >&2 'Seems like source is already modified, aborting test\n'
	exit 0
fi

scripts/fix-spelling
git diff --exit-code

succeed_if 'Please commit the spelling fixes before pushing.
If one of the spelling fixes was incorrect, then please update `/scripts/sed` accordingly.'
printf '\n'

end_script
