@INCLUDE_COMMON@

echo
echo ELEKTRA RACE TESTS
echo

check_version

RACE="@RACE_COMMAND@"

kdb rm -r user/test/race/keys

SHOULD=`$RACE 20 20 400 | grep won | wc -l`
IS=`kdb ls user/test/race/keys | wc -l`

[ "x$SHOULD" = "x$IS" ]
succeed_if "The resolver has a race condition: $SHOULD does not equal $IS"

kdb rm -r user/test/race/keys

end_script
