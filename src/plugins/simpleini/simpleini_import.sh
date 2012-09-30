#!/bin/sh

@INCLUDE_COMMON@

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

$KDB rm -r $ROOT
succeed_if "Could not remove root"

end_script
