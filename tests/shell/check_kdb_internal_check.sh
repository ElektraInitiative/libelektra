#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA KDB INTERNAL TEST SUITE
echo

check_version

FILE="$(mktempfile_elektra)"
cleanup() {
	rm -f "$FILE"
}

printf "Running checks with >%s<\n" "$KDB"

ACTUAL_PLUGINS=$PLUGINS
# Otherwise the test would fail as SHARED_ONLY plugins are not
# available in full and static builds
if contains "$KDB" "full" || contains "$KDB" "static"; then
	ACTUAL_PLUGINS=$ADDED_PLUGINS_WITHOUT_ONLY_SHARED
fi

printf "Checking %s\n\n" "$ACTUAL_PLUGINS"

for PLUGIN in $ACTUAL_PLUGINS; do
	ARGS=""
	case "$PLUGIN" in
	# exclude plugins with known issues
	esac

	ASAN='@ENABLE_ASAN@'
	if [ "$ASAN" = 'ON' ]; then
		# Do not check plugins with known memory leaks in ASAN enabled build
		"$KDB" plugin-info "$PLUGIN" status 2> /dev/null | grep -E -q 'memleak' && continue
	fi

	truncate -s0 "$FILE"
	# shellcheck disable=SC2086
	"$KDB" plugin-check $ARGS "$PLUGIN" > "$FILE" 2>&1
	succeed_if "check of plugin $PLUGIN with args '$ARGS' failed"

	if [ -s "$FILE" ]; then
		echo "check of plugin $PLUGIN produced:"
		cat "$FILE"
		echo
	fi
done

end_script basic commands
