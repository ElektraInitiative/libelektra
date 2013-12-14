@INCLUDE_COMMON@

echo
echo ELEKTRA EXPORT SCRIPTS TESTS
echo

check_version

ROOT=$USER_ROOT
FILE=`mktemp`
PLUGIN=simpleini
DATADIR=@CMAKE_CURRENT_BINARY_DIR@/data

cleanup()
{
	rm -f $FILE
}

$KDB set $ROOT "root" >/dev/null
exit_if_fail "could not set root"

test `$KDB ls $ROOT` = $ROOT
succeed_if "Root key not found"

$KDB export $ROOT $PLUGIN > $FILE
succeed_if "Could not run kdb export"

diff $DATADIR/one_value.simpleini $FILE
succeed_if "Export file one_value.simpleini was not equal"


test "`$KDB set $ROOT/key "value"`" = "create a new key $ROOT/key with string value"
succeed_if "Could not set $ROOT/key"

$KDB export $ROOT $PLUGIN > $FILE
succeed_if "Could not run kdb export"

diff $DATADIR/two_value.simpleini $FILE
succeed_if "Export file two_value.simpleini was not equal"


$KDB set $ROOT/key/subkey "another value" > /dev/null
succeed_if "Could not set $ROOT/key/subkey"

$KDB export $ROOT $PLUGIN > $FILE
succeed_if "Could not run kdb export"

diff $DATADIR/three_value.simpleini $FILE
succeed_if "Export file three_value.simpleini was not equal"


$KDB rm $ROOT/key > /dev/null
succeed_if "Could not rm $ROOT/key"

$KDB export $ROOT $PLUGIN > $FILE
succeed_if "Could not run kdb export"

diff $DATADIR/again_two_value.simpleini $FILE
succeed_if "Export file again_two_value.simpleini was not equal"


$KDB rm -r $ROOT
succeed_if "Could not remove root"

end_script
