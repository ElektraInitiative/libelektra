@INCLUDE_COMMON@

echo
echo ELEKTRA RACE TESTS
echo

check_version

RACE="@RACE_COMMAND@"
RACEKEYS=user/test/race/keys

if [ "x`$KDB ls $RACEKEYS | wc -l 2> /dev/null`" = "x0" ]
then
	echo "There are already keys in $RACEKEYS, I will skip the test"
	exit 0
fi

SHOULD=`$RACE 20 20 400 | grep won | wc -l`
IS=`kdb ls user/test/race/keys | wc -l`

[ "x$SHOULD" = "x$IS" ] && echo "The resolver might have a race condition: $SHOULD does not equal $IS"

$KDB rm -r user/test/race/keys

end_script
