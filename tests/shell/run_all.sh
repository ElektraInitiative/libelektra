#!/bin/sh

@INCLUDE_COMMON@

echo
echo RUN ALL TESTS
echo

check_version
echo

cd "@CMAKE_INSTALL_PREFIX@/@TARGET_TOOL_EXEC_FOLDER@" || exit

nbTests="0"
nbFailed=""

cleanup() {
	rm -rf "$EXPORT_DIR"
}

EXPORT_DIR="$(mktempdir_elektra)"
export_config "$EXPORT_DIR"

# Parse optional argument `-v`
OPTIND=1
while getopts "v" option; do
	case "$option" in
	v)
		verbose=0
		;;
	esac
done
shift "$((OPTIND - 1))"

for t in test* check*; do
	if [ "$t" = "testmod_zeromqsend" ]; then
		echo
		printf "Skipping $t, see 'doc/todo/TESTING'\n"
		echo
		continue
	fi

	echo "Running $t"

	OUTPUT="$("$KDB" "$t" 2>&1)"
	status=$?

	if [ $status != 0 ] || [ $verbose ]; then
		echo
		printf '%s' "$OUTPUT"
		echo
	fi

	if [ $status != "0" ]; then
		nbError=$((nbError + 1))
		nbFailed="$nbFailed\n$t"
		echo error: "$t"
		echo
	fi
	nbTests=$((nbTests + 1))

	export_check "$EXPORT_DIR" "$t"
done

if [ $nbError != "0" ]; then
	echo
	printf "Following test cases failed: $nbFailed\n"
	echo
fi

# fake the number of tests:
nbTest=$nbTests

end_script all
