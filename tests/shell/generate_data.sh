#!/bin/sh

@INCLUDE_COMMON@

#comment out next line to re-generate test data
exit 0

echo
echo ELEKTRA GENERATE DATA
echo

check_version

ROOT=$USER_ROOT

for PLUGIN in $PLUGINS; do
	if is_not_rw_storage; then
		echo "$PLUGIN not a read-write storage"
		continue
	fi

	echo -------- "$PLUGIN" -----------

	"$KDB" set "$ROOT" "root" > /dev/null
	exit_if_fail "could not set root"

	test "$("$KDB" ls "$ROOT")" = "$ROOT"
	succeed_if "Root key not found"

	"$KDB" export "$ROOT" "$PLUGIN" > "$DATADIR"/one_value."$PLUGIN"
	succeed_if "Could not run kdb export"

	test "$("$KDB" set "$ROOT"/key "value")" = "Create a new key $ROOT/key with string \"value\""
	succeed_if "Could not set $ROOT/key"

	"$KDB" export "$ROOT" "$PLUGIN" > "$DATADIR"/two_value."$PLUGIN"
	succeed_if "Could not run kdb export"

	"$KDB" set "$ROOT"/key/subkey "another value" > /dev/null
	succeed_if "Could not set $ROOT/key/subkey"

	"$KDB" export "$ROOT" "$PLUGIN" > "$DATADIR"/three_value."$PLUGIN"
	succeed_if "Could not run kdb export"

	"$KDB" rm "$ROOT"/key > /dev/null
	succeed_if "Could not rm $ROOT/key"

	"$KDB" export "$ROOT" "$PLUGIN" > "$DATADIR"/again_two_value."$PLUGIN"
	succeed_if "Could not run kdb export"

	"$KDB" rm -r "$ROOT"
	succeed_if "Could not remove root"
done

end_script
