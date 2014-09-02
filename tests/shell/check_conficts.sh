@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK CONFLICTS SCRIPTS TESTS
echo

TTY=`tty`

echo -n "Searching for tty: "
tty
RET=$?

#needed to have job control:
# set: can't access tty; job control turned off
if [ $RET -eq 0 ]; then
	if [ -w "$TTY" ]; then
		echo "tty found successfully and is writeable"
	else
		echo "tty found, but tty is not writeable"
		exit 1
	fi
else
	echo "no tty found, wont run test"
	exit 0
fi

set -m

check_version

# simulates a config file
FILE=`mktemp`

# will contain the provoked errors
# this file MUST NOT be equal to $FILE as it seems to cause
# a race condition between kdb set and the bash stderr redirection
ERRORFILE=`mktemp`

# could be any plugin having "c" base variant
PLUGIN=resolver_c_b_b

cleanup()
{
    $KDB umount $SYSTEM_ROOT >/dev/null
    [ $? = "0" ]
    succeed_if "could not umount $SYSTEM_ROOT"
    rm -f $FILE
    rm -f $ERRORFILE
}


if is_plugin_available $PLUGIN
then
    # make sure the mountpoint does not exist
    $KDB umount $SYSTEM_ROOT 1> /dev/null 2> /dev/null
    
    # mount the simulated file with the debug resolver
    $KDB mount --resolver $PLUGIN $FILE $SYSTEM_ROOT
	[ $? = "0" ]
	exit_if_fail "could not mount $FILE at $SYSTEM_ROOT using $PLUGIN"
else
	echo "Aborting tests because required plugin $PLUGIN is missing"
	exit 0
fi

# heat up the config file (create it)
$KDB set $SYSTEM_ROOT value1  
fg %1  
fg %1  
[ $? = "0" ]
succeed_if "unable to initially create the config file"


[ "x`$KDB get $SYSTEM_ROOT 2> /dev/null`" = "xvalue1" ]
succeed_if "could not get correct value1"

echo "Doing first test before locking happens"

$KDB set -v $SYSTEM_ROOT value2 1>/dev/null 2>/dev/null
$KDB set -v $SYSTEM_ROOT value3 1>/dev/null 2> $ERRORFILE

#in the non-locking race condition we need a different timestamp
sleep 1

# first value2 is written and should succeed
fg %1 1>/dev/null
fg %1 1>/dev/null
[ $? = "0" ]
succeed_if "value2 could not be written successfully"

# now try to write value3 and fail because of a race condition
fg %2 1>/dev/null
fg %2 1>/dev/null
[ $? != "0" ]
succeed_if "should fail (time mismatch error)"

# test if the command failed because of the correct reason
grep '(#30)' $ERRORFILE > /dev/null
succeed_if "error number not correct"
grep 'found conflict' $ERRORFILE > /dev/null
succeed_if "error message not correct"

# test if value2 was actually written
[ "x`$KDB get $SYSTEM_ROOT 2> /dev/null`" = "xvalue2" ]
succeed_if "could not get correct value2"

echo "Doing second test during locking"

$KDB set -v $SYSTEM_ROOT value4 1>/dev/null 2>/dev/null
$KDB set -v $SYSTEM_ROOT value5 1>/dev/null 2> $ERRORFILE

# let the value4 set lock the file
fg %1 1> /dev/null

# now let the value5 set lock the file and fail because it is already locked
fg %2 1> /dev/null
[ $? != "0" ]
succeed_if "should fail (unable to get lock)"

# now actually write value4
fg %1
[ $? = "0" ]
succeed_if "value4 could not be written"

# test if the command failed because of the correct reason
grep '(#30)' $ERRORFILE > /dev/null
succeed_if "error number not correct"
grep 'found conflict' $ERRORFILE > /dev/null
succeed_if "error message not correct"

# test if value4 was actually written
[ "x`$KDB get $SYSTEM_ROOT 2> /dev/null`" = "xvalue4" ]
succeed_if "could not get correct value4"

$KDB rm $SYSTEM_ROOT 1>/dev/null 2>/dev/null
fg %1 1> /dev/null
fg %1 1> /dev/null
[ $? = "0" ]
succeed_if "remove should be successful"

cleanup

end_script check conflicts
