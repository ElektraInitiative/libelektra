#!/bin/sh

@INCLUDE_COMMON@

echo "Import with existing root"

$KDB set $ROOT "root" >/dev/null
exit_if_fail "could not set root"

test `$KDB ls $ROOT` = $ROOT
succeed_if "Root key not found"

$KDB import $ROOT simpleini < @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini
succeed_if "Could not run kdb import"

test "`$KDB ls $ROOT`" = "user/tests/script"
succeed_if "key name not correct"

test "`$KDB get $ROOT`" = root
succeed_if "root value not correct"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini $FILE
succeed_if "Export file one_value.ini was not equal"

$KDB rm -r $ROOT
succeed_if "Could not remove root"



echo "Import with empty root"

$KDB import $ROOT simpleini < @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini
succeed_if "Could not run kdb import"

test "`$KDB ls $ROOT`" = "user/tests/script"
succeed_if "key name not correct"

test "`$KDB get $ROOT`" = root
succeed_if "root value not correct"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini $FILE
succeed_if "Export file one_value.ini was not equal"



echo "Import with wrong root (overwrite)"

$KDB set $ROOT "wrong_root" >/dev/null
exit_if_fail "could not set wrong_root"

$KDB import -s overwrite $ROOT simpleini < @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini
succeed_if "Could not run kdb import"

test "`$KDB ls $ROOT`" = "user/tests/script"
succeed_if "key name not correct"

test "`$KDB get $ROOT`" = root
succeed_if "root value not correct"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini $FILE
succeed_if "Export file one_value.ini was not equal"

$KDB rm -r $ROOT
succeed_if "Could not remove root"



echo "Import two values"

$KDB import $ROOT simpleini < @CMAKE_CURRENT_SOURCE_DIR@/two_value.ini
succeed_if "Could not run kdb import"

test "`$KDB ls $ROOT`" = "user/tests/script
user/tests/script/key"
succeed_if "key name not correct"

test "`$KDB get $ROOT`" = root
succeed_if "root value not correct"

test "`$KDB get $ROOT/key`" = value
succeed_if "key value not correct"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/two_value.ini $FILE
succeed_if "Export file two_value.ini was not equal"



echo "Import one value (cut two values from previous test case)"

$KDB import -s overwrite $ROOT simpleini < @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini
succeed_if "Could not run kdb import"

test "`$KDB ls $ROOT`" = "user/tests/script"
succeed_if "key name not correct"

test "`$KDB get $ROOT`" = root
succeed_if "root value not correct"

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.ini $FILE
succeed_if "Export file one_value.ini was not equal"



$KDB rm -r $ROOT
succeed_if "Could not remove root"

end_script
