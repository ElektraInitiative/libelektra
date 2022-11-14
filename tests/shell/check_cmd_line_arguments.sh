#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK CMD LINE ARGUMENTS
echo

check_version

ROOT=user:/tests

if ! is_plugin_available dump; then
	echo "Need dump to run test, will abort"
	exit 0
fi

"$KDB" set $ROOT/export test > /dev/null
"$KDB" export -z $ROOT
[ $? = 1 ]
exit_if_fail "invalid options"

"$KDB" export -vd $ROOT
[ $? = 0 ]
exit_if_fail "Must accept v and d options"

sed -i 's/ksNew/invalidCommand/g' $("$KDB" file $ROOT)
succeed_if "Cannot corrupt dump file"

TMPFILE="$(mktempfile_elektra)"
cleanup() {
	rm -f "$TMPFILE"
}

"$KDB" export -vd $ROOT > "$TMPFILE" 2>&1

sed -i 1,7d "$TMPFILE"
CONTENT=$(cat "$TMPFILE")

grep "Mountpoint:" "$TMPFILE" > /dev/null
succeed_if "Verbose output does not work, got $CONTENT"

grep "At:" "$TMPFILE" > /dev/null
succeed_if "Debug output does not work, got $CONTENT"

rm -f "$TMPFILE"
rm -f $("$KDB" file $ROOT)
