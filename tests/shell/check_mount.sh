@INCLUDE_COMMON@

echo
echo ELEKTRA MOUNT TESTS
echo

is_plugin_available sync || {
	echo "Test requires sync plugin, aborting" >&2
	exit 0
}
check_version

echo "Test basic mounting"

ROOT_FILE=${FILE_SUFFIX}_root.ecf
ROOT_MOUNTPOINT=/test/script/mount
ROOT_MOUNTPOINTN=\\/test\\/script\\/mount
ROOT_MOUNTPOINT2=/test/script/remount
ROOT_MOUNTPOINT2N=\\/test\\/script\\/remount

if is_plugin_available dump; then
	"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT dump 1> /dev/null
	succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT"

	"$KDB" umount $ROOT_MOUNTPOINT
	succeed_if "could not unmount previously mounted mountpoint"
fi

# TODO [new_backend]: tests disabled
# if is_plugin_available tracer; then
# 	if is_plugin_available sync; then
# 		"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT tracer sync 1> /dev/null 2> /dev/null
# 		[ $? != 0 ]
# 		succeed_if "could mount the backend, even though tracer+sync conflict"
#
# 		"$KDB" umount $ROOT_MOUNTPOINT
# 		[ $? != 0 ]
# 		succeed_if "could unmount what should not be mounted"
# 	fi
# fi

echo "Test mounting plugin stack"

if is_plugin_available hosts; then
	if is_plugin_available glob; then
		"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT glob hosts 1> /dev/null
		succeed_if "could not mount glob and hosts plugin together"

		"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT glob hosts 1> /dev/null 2> /dev/null
		[ $? != 0 ]
		succeed_if "could remount the same backend"

		"$KDB" mount $ROOT_FILE dir:$ROOT_MOUNTPOINT glob hosts 1> /dev/null 2> /dev/null
		[ $? != 0 ]
		succeed_if "could remount the dir backend, even though cascading already mounted"

		"$KDB" mount $ROOT_FILE user:$ROOT_MOUNTPOINT glob hosts 1> /dev/null 2> /dev/null
		[ $? != 0 ]
		succeed_if "could remount the user backend, even though cascading already mounted"

		"$KDB" mount $ROOT_FILE system:$ROOT_MOUNTPOINT glob hosts 1> /dev/null 2> /dev/null
		[ $? != 0 ]
		succeed_if "could remount the system backend, even though cascading already mounted"

		"$KDB" umount $ROOT_MOUNTPOINT
		succeed_if "could not unmount previously mounted mountpoint"

		echo "Test simple mount configuration"

		"$KDB" mount -c "test1=testvalue1" $ROOT_FILE $ROOT_MOUNTPOINT glob "test1=testvalue1" hosts 1> /dev/null
		succeed_if "could not mount glob and hosts plugin together"

		#"$KDB" ls "system:/elektra/mountpoints/$ROOT_MOUNTPOINTN"

		# TODO: check correcly + reenable
		configvalue=$("$KDB" get "system:/elektra/mountpoints/$ROOT_MOUNTPOINTN/config/test1")
		test "$configvalue" = "testvalue1"
		succeed_if "config key was not set correctly"

		"$KDB" umount $ROOT_MOUNTPOINT
		succeed_if "could not unmount previously mounted mountpoint"

		echo "Test multiple mount configurations"

		"$KDB" mount -c "test1=testvalue1,test2=test value2" $ROOT_FILE $ROOT_MOUNTPOINT glob "test1=testvalue1" hosts "test2=test value2" 1> /dev/null
		succeed_if "could not mount glob and hosts plugin together"

		# TODO: reenable
		configvalue=$("$KDB" get "system:/elektra/mountpoints/$ROOT_MOUNTPOINTN/config/test2")
		test "$configvalue" = "test value2"
		succeed_if "config key was not set correctly"

		echo "Test remounting the existing mount"

		"$KDB" remount "testfile" $ROOT_MOUNTPOINT2 $ROOT_MOUNTPOINT
		succeed_if "could not remount previous mountpoint"

		#$KDB ls "system:/elektra/mountpoints"
		#$KDB mount

		"$KDB" umount $ROOT_MOUNTPOINT
		succeed_if "could not unmount previously mounted mountpoint"

		"$KDB" ls "system:/elektra/mountpoints/$ROOT_MOUNTPOINT2N/" | grep glob 1> /dev/null
		succeed_if "glob plugin does not exist in the remounted mountpoint"

		"$KDB" ls "system:/elektra/mountpoints/$ROOT_MOUNTPOINT2N/" | grep hosts 1> /dev/null
		succeed_if "hosts plugin does not exist in the remounted mountpoint"

		# TODO: reenable
		#configvalue=$($KDB get "system:/elektra/mountpoints/$ROOT_MOUNTPOINT2N/config/test2")
		#test "$configvalue" = "test value2"
		#succeed_if "config key was not copied correctly"

		#configvalue=$($KDB get "system:/elektra/mountpoints/$ROOT_MOUNTPOINT2N/config/path")
		#test "$configvalue" = "testfile"
		#succeed_if "path was not set correctly"

		echo "Testing unmount via path"

		"$KDB" umount $ROOT_MOUNTPOINT2
		succeed_if "unable to unmount $ROOT_MOUNTPOINT2"

	fi
fi

end_script
