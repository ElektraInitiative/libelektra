#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA BASIC COMMAND SCRIPTS TESTS
echo

echo "testing set and get commands"

#$KDB mount $FILE $ROOT >/dev/null
#succeed_if "could not mount $FILE at $ROOT"

$KDB set $ROOT "root" >/dev/null
exit_if_fail "could not set root"

#[ -f $FOLDER/$FILE ]
#succeed_if "$FOLDER/$FILE does not exist"

[ "x`$KDB get $ROOT 2> /dev/null`" = "xroot" ]
succeed_if "could not set root"

$KDB set "$ROOT/value" "$VALUE" >/dev/null
exit_if_fail "could not set value"

[ "x`$KDB get $ROOT/value 2> /dev/null`" = "x$VALUE" ]
exit_if_fail "cant get $ROOT/value"

echo "testing ls command"

[ "x`$KDB ls $ROOT/value 2> /dev/null`" = "x$ROOT/value" ]
succeed_if "cant ls $ROOT (may mean that $ROOT folder is not clean)"

echo "testing rm command"

$KDB rm $ROOT/value
succeed_if "could not remove user/test/value"

$KDB get $ROOT/value 2>/dev/null
[ $? != "0" ]
succeed_if "got removed key $ROOT/value"

$KDB rm $ROOT >/dev/null 2>/dev/null
succeed_if "could not remove user/test/value"

$KDB get $ROOT/value 2> /dev/null
[ $? != "0" ]
succeed_if "got removed key $ROOT"

#[ ! -f $FOLDER/$FILE ]
#succeed_if "$FOLDER/$FILE still exist"

end_script
