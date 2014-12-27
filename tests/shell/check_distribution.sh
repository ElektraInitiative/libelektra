@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK DISTRIBUTION
echo

# high level test to check whether keys are distributed to correct
# backend

check_version

#method that does all the checking
check_distribution()
{
	echo "Check distribution of $1 and $2"
	MOUNTPOINT1="$1"
	MOUNTPOINT2="$2"
	FILE1="/tmp/file1"
	FILE2="/tmp/file2"
	VALUE1="value111111"
	VALUE2="value222222"

	[ ! -e $FILE1 ]
	exit_if_fail $FILE1 already exists

	[ ! -e $FILE2 ]
	exit_if_fail $FILE2 already exists

	$KDB mount $FILE1 $MOUNTPOINT1 $KDB_DEFAULT_STORAGE 1>/dev/null
	succeed_if "could not mount 1: $FILE1 at $MOUNTPOINT1"

	$KDB mount $FILE2 $MOUNTPOINT2 $KDB_DEFAULT_STORAGE 1>/dev/null
	succeed_if "could not mount 2: $FILE2 at $MOUNTPOINT2"

	FILE=`$KDB file -n $MOUNTPOINT1`
	[ "x$FILE"  = "x$FILE1" ]
	succeed_if "resolving of $MOUNTPOINT1 did not yield $FILE1 but $FILE"

	FILE=`$KDB file -n $MOUNTPOINT2`
	[ "x$FILE"  = "x$FILE2" ]
	succeed_if "resolving of $MOUNTPOINT2 did not yield $FILE2 but $FILE"

	KEY1=$MOUNTPOINT1/key
	$KDB set $KEY1 $VALUE1 > /dev/null
	succeed_if "could not set $KEY1"

	KEY2=$MOUNTPOINT2/key
	$KDB set $KEY2 $VALUE2 > /dev/null
	succeed_if "could not set $KEY2"

	[ "x`$KDB sget $KEY1 defvalue 2> /dev/null`" = "x$VALUE1" ]
	succeed_if "Did not get value $VALUE1 for $KEY1"

	[ "x`$KDB sget $KEY2 defvalue 2> /dev/null`" = "x$VALUE2" ]
	succeed_if "Did not get value $VALUE2 for $KEY2"

	grep $VALUE1 $FILE1 >/dev/null
	succeed_if "did not find $VALUE1 within $FILE1"

	grep $VALUE2 $FILE2 >/dev/null
	succeed_if "did not find $VALUE2 within $FILE2"

	$KDB umount $MOUNTPOINT1 >/dev/null
	succeed_if "could not umount $MOUNTPOINT1"

	$KDB umount $MOUNTPOINT2 >/dev/null
	succeed_if "could not umount $MOUNTPOINT2"

	rm -f $FILE1
	rm -f $FILE2
}

check_distribution system$MOUNTPOINT/distribution/a1 system$MOUNTPOINT/distribution/b2
#TODO Bug: double slash leads to (null) as file name:
#check_distribution system/$MOUNTPOINT/distribution/a1 system/$MOUNTPOINT/distribution/b2
#TODO Not checked:
#check_distribution system$MOUNTPOINT/distribution system$MOUNTPOINT/distribution/b2
#check_distribution system$MOUNTPOINT/distribution/a1 system$MOUNTPOINT/distribution

end_script resolver
