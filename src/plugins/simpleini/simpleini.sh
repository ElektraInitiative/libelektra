#!/bin/sh

@INCLUDE_COMMON@

$KDB set $ROOT "root" >/dev/null
exit_if_fail "could not set root"

$KDB ls $ROOT

FILE=`mktemp`
$KDB export $ROOT simpleini > $FILE
succeed_if "Could not run kdb export"

diff @CMAKE_CURRENT_SOURCE_DIR@/one_value.cmp $FILE
succeed_if "Export file one_value.cmp was not equal"

$KDB rm -r $ROOT

end_script
