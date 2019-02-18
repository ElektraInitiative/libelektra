@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK FORMATTING
echo

command -v git > /dev/null 2>&1 || {
	echo "git command needed for this test, aborting" >&2
	exit 0
}

cd "@CMAKE_SOURCE_DIR@"

if ! git diff --exit-code; then
	echo "Seems like source is already modified, aborting test" >&2
	exit 0
fi

scripts/reformat-source || echo 'Warning: clang-format not available, skipping reformat-source' &
scripts/reformat-cmake || echo 'Warning: Unable to reformat CMake code.' &
scripts/reformat-markdown || echo 'Warning: Unable to reformat Markdown code.' &
scripts/reformat-shfmt || echo 'Warning: Unable to reformat Shell code.' &
wait

git diff --exit-code
succeed_if "Please commit the reformatting changes before pushing"

end_script
