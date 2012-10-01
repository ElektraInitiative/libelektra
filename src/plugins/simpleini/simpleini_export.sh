#!/bin/sh

@INCLUDE_COMMON@

$KDB set $ROOT "root" >/dev/null
exit_if_fail "could not set root"

test `$KDB ls $ROOT` = $ROOT
succeed_if "Root key not found"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini $FILE
succeed_if "Export file one_value.ini was not equal"


test "`$KDB set $ROOT/key "value"`" = "create a new key $ROOT/key with string value"
succeed_if "Could not set $ROOT/key"

$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/two_value.ini $FILE
succeed_if "Export file two_value.ini was not equal"


$KDB set $ROOT/key/subkey "another value" > /dev/null
succeed_if "Could not set $ROOT/key/subkey"

$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/three_value.ini $FILE
succeed_if "Export file three_value.ini was not equal"


$KDB rm $ROOT/key > /dev/null
succeed_if "Could not rm $ROOT/key"

$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/again_two_value.ini $FILE
succeed_if "Export file again_two_value.ini was not equal"


$KDB rm -r $ROOT
succeed_if "Could not remove root"

end_script
