#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK DISTRIBUTION
echo

# Adds the specified namespace to a key name if the key does not have one.
# If the key already has a namespace, the namespace remains unchanged.
# The result is printed to stdout.
# $1 ... namespace
# $2 ... key name
prepend_namespace_if_not_present() {
	local present_namespace=$("$KDB" namespace $2)

	if [ -z "$present_namespace" ]; then
		echo $1:$2
	else
		echo $2
	fi
}

# high level test to check whether keys are distributed to correct
# backend

is_plugin_available sync || {
	echo "Test requires sync plugin, aborting" >&2
	exit 0
}
check_version

#method that does all the checking
check_distribution() {
	echo "  Check distribution of $1 and $2"
	MOUNTPOINT1="$1"
	MOUNTPOINT2="$2"
	FILE1=$(mktemp /tmp/file1XXXXXX)
	FILE2=$(mktemp /tmp/file2XXXXXX)
	VALUE1="value111111"
	VALUE2="value222222"

	"$KDB" mount $FILE1 $MOUNTPOINT1 $KDB_DEFAULT_STORAGE 1> /dev/null
	succeed_if "could not mount 1: $FILE1 at $MOUNTPOINT1"

	"$KDB" mount $FILE2 $MOUNTPOINT2 $KDB_DEFAULT_STORAGE 1> /dev/null
	succeed_if "could not mount 2: $FILE2 at $MOUNTPOINT2"

	FILE=$("$KDB" file -n $(prepend_namespace_if_not_present system $MOUNTPOINT1))
	[ "x$FILE" = "x$FILE1" ]
	succeed_if "resolving of $MOUNTPOINT1 did not yield $FILE1 but $FILE"

	FILE=$("$KDB" file -n $(prepend_namespace_if_not_present system $MOUNTPOINT1/xxx))
	[ "x$FILE" = "x$FILE1" ]
	succeed_if "resolving of $MOUNTPOINT1/xxx did not yield $FILE1 but $FILE"

	FILE=$("$KDB" file -n $(prepend_namespace_if_not_present system $MOUNTPOINT2))
	[ "x$FILE" = "x$FILE2" ]
	succeed_if "resolving of $MOUNTPOINT2 did not yield $FILE2 but $FILE"

	FILE=$("$KDB" file -n $(prepend_namespace_if_not_present system $MOUNTPOINT2/xxx))
	[ "x$FILE" = "x$FILE2" ]
	succeed_if "resolving of $MOUNTPOINT2/xxx did not yield $FILE2 but $FILE"

	KEY1=$MOUNTPOINT1/key
	"$KDB" set $(prepend_namespace_if_not_present system $KEY1) $VALUE1 > /dev/null
	succeed_if "could not set $KEY1"

	KEY2=$MOUNTPOINT2/key
	"$KDB" set $(prepend_namespace_if_not_present system $KEY2) $VALUE2 > /dev/null
	succeed_if "could not set $KEY2"

	[ "x$("$KDB" sget $KEY1 defvalue 2> /dev/null)" = "x$VALUE1" ]
	succeed_if "Did not get value $VALUE1 for $KEY1"

	[ "x$("$KDB" sget $KEY2 defvalue 2> /dev/null)" = "x$VALUE2" ]
	succeed_if "Did not get value $VALUE2 for $KEY2"

	grep $VALUE1 $FILE1 > /dev/null
	succeed_if "did not find $VALUE1 within $FILE1"

	grep $VALUE2 $FILE2 > /dev/null
	succeed_if "did not find $VALUE2 within $FILE2"

	"$KDB" rm $KEY1
	succeed_if "Could not remove $KEY1"

	"$KDB" rm $KEY2
	succeed_if "Could not remove $KEY2"

	"$KDB" umount $MOUNTPOINT1 > /dev/null
	succeed_if "could not umount $MOUNTPOINT1"

	"$KDB" umount $MOUNTPOINT2 > /dev/null
	succeed_if "could not umount $MOUNTPOINT2"

	rm -f $FILE1
	rm -f $FILE2
}

echo "Testing sibling"
check_distribution system:$MOUNTPOINT/distribution/a1 system:$MOUNTPOINT/distribution/b2
check_distribution system:/$MOUNTPOINT/distribution/a1 system:/$MOUNTPOINT/distribution/b2
check_distribution system:////$MOUNTPOINT/distribution///a1 system://///$MOUNTPOINT/distribution////b2

echo "Testing direct below"
check_distribution system:$MOUNTPOINT/distribution system:$MOUNTPOINT/distribution/b2
check_distribution system:$MOUNTPOINT/distribution/a1 system:$MOUNTPOINT/distribution
check_distribution system:///$MOUNTPOINT///distribution system://$MOUNTPOINT///distribution///b2
check_distribution system://$MOUNTPOINT///distribution///a1 system://$MOUNTPOINT///distribution

echo "Testing below"
check_distribution system:$MOUNTPOINT/distribution system:$MOUNTPOINT/distribution/b2/more/below
check_distribution system:$MOUNTPOINT/distribution/a1/more/below system:$MOUNTPOINT/distribution
check_distribution system:///$MOUNTPOINT////distribution system://$MOUNTPOINT//distribution/b2///more///below
check_distribution system:///$MOUNTPOINT//distribution///a1/more///below system://$MOUNTPOINT////distribution

if [ "x$WRITE_TO_SYSTEM" = "xYES" ]; then
	echo "Testing root with normal"
	check_distribution / system:$MOUNTPOINT/distribution
	check_distribution / system://$MOUNTPOINT////distribution
	check_distribution system:$MOUNTPOINT/distribution /
	check_distribution system://$MOUNTPOINT////distribution /

	echo "Testing root with cascading"
	check_distribution / $MOUNTPOINT/distribution
	check_distribution / $MOUNTPOINT////distribution
	check_distribution $MOUNTPOINT/distribution /
	check_distribution //$MOUNTPOINT////distribution /
else
	echo "Excluded tests with root, set WRITE_TO_SYSTEM=YES to include them"
fi

echo "Testing cascading with normal"
check_distribution $MOUNTPOINT/distribution/a1 system:$MOUNTPOINT/distribution/b2
check_distribution $MOUNTPOINT/distribution system:$MOUNTPOINT/distribution/b2
check_distribution $MOUNTPOINT/distribution/a1 system:$MOUNTPOINT/distribution
check_distribution $MOUNTPOINT/distribution/a1//deep///below system:$MOUNTPOINT/distribution

echo "Testing cascading with cascading"
check_distribution $MOUNTPOINT/distribution/a1 $MOUNTPOINT/distribution/b2
check_distribution $MOUNTPOINT/distribution $MOUNTPOINT/distribution/b2
check_distribution $MOUNTPOINT/distribution/a1 $MOUNTPOINT/distribution
check_distribution $MOUNTPOINT/distribution/a1//deep///below $MOUNTPOINT/distribution

end_script resolver
