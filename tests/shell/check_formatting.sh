@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK FORMATTING
echo

command -v git > /dev/null 2>&1 || {
	printf >&2 'This test requires the `git` command, aborting test!\n\n'
	exit 0
}

cd "@CMAKE_SOURCE_DIR@"

if ! git diff --quiet; then
	printf >&2 'Source is already modified, aborting test!\n\n'
	exit 0
fi

reformat() {
	reformat_command=$1
	reformat_command_output="$(scripts/$reformat_command 2>&1)" || {
		printf >&2 -- '————————————————————————————————————————————————————————————\n'
		printf >&2 -- 'Warning — Reformatting command `%s` failed\n' "$reformat_command"
		printf >&2 -- '\n%s\n' "$reformat_command_output"
		printf >&2 -- '————————————————————————————————————————————————————————————\n\n'
	}
}

reformat reformat-source &
reformat reformat-cmake &
reformat reformat-markdown &
reformat reformat-shfmt &
wait

git diff --exit-code
succeed_if "Please commit the reformatting changes before pushing"

end_script
