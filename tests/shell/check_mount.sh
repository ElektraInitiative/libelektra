@INCLUDE_COMMON@

echo
echo ELEKTRA MOUNT TESTS
echo

check_version

echo "Test basic mounting"

ROOT_FILE=${FILE_SUFFIX}_root.ecf
ROOT_MOUNTPOINT=/test/script/mount
ROOT_MOUNTNAME=_test_script_mount
ROOT_MOUNTPOINT2=/test/script/remount
ROOT_MOUNTNAME2=_test_script_remount

if is_plugin_available dump
then
	$KDB mount $ROOT_FILE $ROOT_MOUNTPOINT dump 1>/dev/null
	succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT"
	
	$KDB umount $ROOT_MOUNTNAME
	succeed_if "could not unmount previously mounted mountpoint"
fi

echo "Test mounting plugin stack"

if is_plugin_available hosts
then
	if is_plugin_available glob
	then
		$KDB mount $ROOT_FILE $ROOT_MOUNTPOINT glob hosts 1>/dev/null
		succeed_if "could not mount glob and hosts plugin together"

		$KDB umount $ROOT_MOUNTNAME
		succeed_if "could not unmount previously mounted mountpoint"		

		echo "Test simple mount configuration"
		
		$KDB mount $ROOT_FILE $ROOT_MOUNTPOINT glob "test1=testvalue1" hosts 1>/dev/null
		succeed_if "could not mount glob and hosts plugin together"				
		
		configvalue=$($KDB get "system/elektra/mountpoints/$ROOT_MOUNTNAME/config/test1")
		test "$configvalue" = "testvalue1"
		succeed_if "config key was not set correctly"
		
		$KDB umount $ROOT_MOUNTNAME
		succeed_if "could not unmount previously mounted mountpoint"
		
		echo "Test multiple mount configurations"
		
		$KDB mount $ROOT_FILE $ROOT_MOUNTPOINT glob "test1=testvalue1" hosts "test2=test value2" 1>/dev/null
		succeed_if "could not mount glob and hosts plugin together"				
		
		configvalue=$($KDB get "system/elektra/mountpoints/$ROOT_MOUNTNAME/config/test2")
		test "$configvalue" = "test value2" 
		succeed_if "config key was not set correctly"
		
		echo "Test remounting the existing mount"
		
		$KDB remount "testfile" $ROOT_MOUNTPOINT2 $ROOT_MOUNTNAME
        succeed_if "could not remount previous mountpoint"             

        $KDB umount $ROOT_MOUNTNAME
        succeed_if "could not unmount previously mounted mountpoint"

        $KDB ls "system/elektra/mountpoints/$ROOT_MOUNTNAME2/" | grep glob 1> /dev/null
        succeed_if "glob plugin does not exist in the remounted mountpoint"

        $KDB ls "system/elektra/mountpoints/$ROOT_MOUNTNAME2/" | grep hosts 1> /dev/null
        succeed_if "hosts plugin does not exist in the remounted mountpoint"

        configvalue=$($KDB get "system/elektra/mountpoints/$ROOT_MOUNTNAME2/config/test2")
        test "$configvalue" = "test value2" 
        succeed_if "config key was not copied correctly"
				
        configvalue=$($KDB get "system/elektra/mountpoints/$ROOT_MOUNTNAME2/config/path")
        test "$configvalue" = "testfile" 
        succeed_if "path was not set correctly"

        echo "Testing unmount via path"
        
        $KDB umount $ROOT_MOUNTPOINT2
        succeed_if "unable to unmount $ROOT_MOUNTPOINT2 via path"
														
	fi
fi






end_script
