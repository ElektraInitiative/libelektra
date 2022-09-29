@INCLUDE_COMMON@

echo
echo ELEKTRA SPEC SET TESTS
echo

check_version

set -x

echo "Test type specification working"

ROOT_FILE=spec_tests_set.ecf
ROOT_MOUNTPOINT=/test/script/specSet

if is_plugin_available dump && is_plugin_available list && is_plugin_available sync && is_plugin_available type; then
	"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT dump type 1> /dev/null
	succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT"

	SYSTEM_FILE="$("$KDB" file -n system:$ROOT_MOUNTPOINT)"
	[ ! -f "$SYSTEM_FILE" ]
	exit_if_fail "System File $SYSTEM_FILE already exists"

	"$KDB" mount $ROOT_FILE spec:$ROOT_MOUNTPOINT dump 1> /dev/null
	succeed_if "could not mount spec root: $ROOT_FILE at spec:$ROOT_MOUNTPOINT"

	SPEC_FILE="$("$KDB" file -n spec:$ROOT_MOUNTPOINT)"
	[ ! -f "$SPEC_FILE" ]
	exit_if_fail "Spec File $SPEC_FILE already exists"

	"$KDB" get $ROOT_MOUNTPOINT
	[ $? != 0 ]
	succeed_if "getting cascading should fail if nothing is there"

	"$KDB" set spec:$ROOT_MOUNTPOINT/test "" > /dev/null
	succeed_if "could not create key"

	"$KDB" get $ROOT_MOUNTPOINT
	[ $? != 0 ]
	succeed_if "getting cascading should fail if nothing is there"

	"$KDB" meta-set spec:$ROOT_MOUNTPOINT/first type unsigned_short
	succeed_if "could not set meta"

	"$KDB" set system:$ROOT_MOUNTPOINT/first abcd
	[ $? != 0 ]
	succeed_if "should complain that value is not a number"

	"$KDB" umount $ROOT_MOUNTPOINT
	succeed_if "could not unmount previously mounted mountpoint"

	"$KDB" umount spec:$ROOT_MOUNTPOINT
	succeed_if "could not unmount previously mounted spec mountpoint"

	rm -f "$SYSTEM_FILE"
	rm -f "$SPEC_FILE"
fi

end_script spec set tests
