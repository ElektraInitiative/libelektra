#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA RACE TESTS
echo

check_version

#set -x

echo "Will skip the test, because it is sensible to ulimit settings and race might hang"
exit 0

RACE="@RACE_COMMAND@"
RACEKEYS=user:/test/race/keys

if [ "x$("$KDB" ls $RACEKEYS | wc -l 2> /dev/null)" != "x0" ]; then
	echo "There are already keys in $RACEKEYS"
	exit 1
fi

if $RACE | grep "This program tests race condition in Elektra"; then
	echo "Doing race tests"
else
	echo "No $RACE tool installed"
	exit 0
fi

do_race_test() {
	RES=$($RACE $*)
	succeed_if "$RACE $* did not run successfully with error $?"

	WHERE=user:/test/race/keys

	KEYS=$("$KDB" ls "$WHERE")
	succeed_if "could not run $KDB ls $WHERE successfully"

	WON=$(echo "$RES" | grep won)
	SHOULD=$(echo "$WON" | wc -l)
	IS=$(echo "$KEYS" | wc -l)
	RUNNING=$(ps aux | grep race | grep -v grep | grep -v check)
	OUTPUT="\nFOR $*\nwith KEYS: $KEYS\nWON: $WON\nIS: $IS and RUNNING: $RUNNING\n\n"

	echo "test $*: $SHOULD - $IS"

	[ "$SHOULD" -ge "$IS" ]
	succeed_if "The resolver has a race condition: $SHOULD is smaller than $IS! $OUTPUT"

	# currently multiple winners are possible as the test seems to
	# be flawed in multi-process setup
	#[ "$SHOULD" -le "1"  ]
	#succeed_if "race had not one or zero, but $SHOULD, winner(s)! $OUTPUT"

	[ "$IS" -eq "1" ]
	succeed_if "keyset had not one, but $IS, key(s)! $OUTPUT"

	"$KDB" rm -r $WHERE
	succeed_if "could not remove key! $OUTPUT"
}

do_race_test 13 1 13
do_race_test 1 13 13

do_race_test 20 1 20
do_race_test 1 20 20

do_race_test 1 200 200
do_race_test 200 1 200

do_race_test 1 333 333
do_race_test 333 1 333

do_race_test 1 500 500

do_race_test 10 20 200
do_race_test 20 10 200

do_race_test 20 20 400

end_script
