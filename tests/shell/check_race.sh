@INCLUDE_COMMON@

echo
echo ELEKTRA RACE TESTS
echo

check_version

#set -x

RACE="@RACE_COMMAND@"
RACEKEYS=user/test/race/keys

if [ "x`$KDB ls $RACEKEYS | wc -l 2> /dev/null`" != "x0" ]
then
	echo "There are already keys in $RACEKEYS, I will skip the test"
	exit 0
fi

echo "Doing race tests"

do_race_test()
{
	SHOULD=`$RACE $* | grep won | wc -l`
	IS=`$KDB ls user/test/race/keys | wc -l`

	echo "$SHOULD - $IS in test $*"

	[ "x$SHOULD" = "x$IS" ] 
	succeed_if "The resolver has a race condition: $SHOULD does not equal $IS for $*"

	[ "x$SHOULD" = "x1" ] 
	succeed_if "race had more than one winner"

	[ "x$IS" = "x1" ] 
	succeed_if "keyset now contains more than one key"

	$KDB rm -r user/test/race/keys
}

do_race_test 13 1 13
do_race_test 1 13 13

do_race_test 20 1 20
do_race_test 1 20 20

do_race_test 1 200 200
do_race_test 200 1 200

do_race_test 1 333 333
do_race_test 333 1 333

end_script
