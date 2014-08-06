@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK CONFLICTS SCRIPTS TESTS
echo

check_version

FILE=`mktemp`
MOUNTPOINT=system/tests/script
MOUNTNAME=system_tests_script
cleanup()
{
	rm -f $FILE
}

PLUGIN=resolver_c_b_b
# could be any plugin having "c" base variant

#TODO kdb mount does not not allow to mount different resolver
exit 0

#needed to have job control:
set -m

if is_plugin_available $PLUGIN
then
	#$KDB mount --resolver $PLUGIN $FILE $MOUNTPOINT 1>/dev/null
	$KDB mount $FILE $MOUNTPOINT 1>/dev/null
	# TODO: might be not needed with --resolver
	fg %1 1>/dev/null 2> /dev/null
	fg %1 1>/dev/null 2> /dev/null
	[ $? = "0" ]
	exit_if_fail "could not mount $FILE at $MOUNTPOINT using $MOUNT_PLUGIN"
else
	echo "Aborting tests because required plugin $PLUGIN is missing"
	exit 0
fi

cleanup()
{
	$KDB umount $MOUNTNAME >/dev/null
	# TODO: might be not needed with --resolver
	fg %1 1>/dev/null 2> /dev/null
	fg %1 1>/dev/null 2> /dev/null
	[ $? = "0" ]
	succeed_if "could not umount $MOUNTNAME"
	rm -f $FILE
}

# heat up the config file (create it)
$KDB set $USER_ROOT value1 1>/dev/null 2>/dev/null
fg %1 1>/dev/null 2> /dev/null
fg %1 1>/dev/null 2> /dev/null
[ $? = "0" ]
succeed_if "should be successful"


[ "x`$KDB get $USER_ROOT 2> /dev/null`" = "xvalue1" ]
succeed_if "could not get correct value1"




echo "Doing first test before locking happens"

$KDB set -v $USER_ROOT value2 1>/dev/null 2>/dev/null
$KDB set -v $USER_ROOT value3 1>/dev/null 2>$FILE

#in the non-locking race condition we need a different timestamp
sleep 1

fg %1 1>/dev/null
fg %1 1>/dev/null
[ $? = "0" ]
succeed_if "should be successful"
fg %2 1>/dev/null
fg %2 1>/dev/null
[ $? != "0" ]
succeed_if "should fail (time mismatch error)"

grep '(#30)' $FILE > /dev/null
succeed_if "error number not correct"
grep 'found conflict' $FILE > /dev/null
succeed_if "error message not correct"

[ "x`$KDB get $USER_ROOT 2> /dev/null`" = "xvalue2" ]
succeed_if "could not get correct value2"







echo "Doing second test during locking"

$KDB set -v $USER_ROOT value4 1>/dev/null 2>/dev/null
$KDB set -v $USER_ROOT value5 1>/dev/null 2> $FILE

fg %1 1> /dev/null
fg %2 1> /dev/null
[ $? != "0" ]
succeed_if "should fail (unable to get lock)"
fg %1
[ $? = "0" ]
succeed_if "should be successful"

grep '(#30)' $FILE > /dev/null
succeed_if "error number not correct"
grep 'found conflict' $FILE > /dev/null
succeed_if "error message not correct"

[ "x`$KDB get $USER_ROOT 2> /dev/null`" = "xvalue4" ]
succeed_if "could not get correct value4"


$KDB rm $USER_ROOT 1>/dev/null 2>/dev/null
fg %1 1> /dev/null
fg %1 1> /dev/null
[ $? = "0" ]
succeed_if "remove should be successful"

cleanup

end_script check conflicts
