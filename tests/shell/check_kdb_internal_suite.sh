#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA KDB INTERNAL TEST SUITE
echo

check_version

#override for specific testing
#PLUGINS="quickdump"

for PLUGIN in $PLUGINS; do
	if is_not_rw_storage; then
		echo "$PLUGIN not a read-write storage"
		continue
	fi

	case "$PLUGIN" in
	"tcl")
		MOUNT_PLUGIN="$PLUGIN ccode null"
		TESTS="basic"
		;;
	"line")
		TESTS="basic"
		;;
	"simpleini")
		MOUNT_PLUGIN="$PLUGIN hexcode null"
		TESTS="basic string"
		;;
	"dump")
		MOUNT_PLUGIN="$PLUGIN"
		# all tests for dump.. (takes long time)
		TESTS=
		# tests taking not soo long:
		TESTS="basic string umlauts binary naming"
		;;
	"quickdump")
		MOUNT_PLUGIN="$PLUGIN"
		TESTS="basic string umlauts binary naming"
		;;
	*)
		MOUNT_PLUGIN="$PLUGIN"
		#if a new plugin can manage something we are happy
		TESTS="basic string"
		;;
	esac

	unset -f cleanup
	FILE=test.$PLUGIN

	check_remaining_files $FILE

	"$KDB" mount $FILE $MOUNTPOINT $MOUNT_PLUGIN 1> /dev/null
	exit_if_fail "could not mount $FILE at $MOUNTPOINT using $MOUNT_PLUGIN"

	cleanup() {
		"$KDB" umount $MOUNTPOINT > /dev/null
		succeed_if "could not umount $MOUNTPOINT"
		rm -f $USER_FOLDER/$FILE
		rm -f $SYSTEM_FOLDER/$FILE

		echo "Cleanup for $PLUGIN"

		USER_REMAINING="$(find $USER_FOLDER -maxdepth 1 -name $FILE'*' -print -exec rm {} +)"
		test -z "$USER_REMAINING"
		succeed_if "found remaining files $USER_REMAINING in $USER_FOLDER"

		SYSTEM_REMAINING="$(find $SYSTEM_FOLDER -maxdepth 1 -name $FILE'*' -print -exec rm {} +)"
		test -z "$SYSTEM_REMAINING"
		succeed_if "found remaining files $SYSTEM_REMAINING in $SYSTEM_FOLDER"
	}

	echo "Running tests for $PLUGIN"
	for ROOT in $USER_ROOT $SYSTEM_ROOT; do
		"$KDB" test "$ROOT" $TESTS
		succeed_if "could not run test suite"
	done

	cleanup
done
unset -f cleanup

end_script basic commands
