#!/bin/sh
#test_script.sh
#shell test suite for kdb command

#variables
nbError=0
nbTest=0

if [ "z$srcdir" = 'z' ]; then
  HOME=.
else
  HOME=$srcdir
fi
export HOME

VALUE="value"
ROOT="user/tests/script"
FILE="$HOME/.kdb/$ROOT/value"
DIR="$HOME/.kdb/$ROOT"
USER="`id -un`"
GROUP="`id -gn`"
DATE="`date \"+%b %d %H:%M\"`"

export KDB_DIR=".kdb"

#define succeed_if(x,y) nbTest++; if (!(x)) { nbError++; printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y);}
#succeed if the previous command was successful
succeed_if ()
{
	if [ $? != "0" ]
	then
		nbError=$(( $nbError + 1 ))
		echo error: $*
	fi
	nbTest=$(( $nbTest + 1 ))
}

#define exit_if_fail(x,y) nbTest++; if (!(x)) { printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); exit(1); }
#fails and exits the program if the previous command failed
exit_if_fail ()
{
	if [ $? != "0" ]
	then
		echo fatal: $*
		exit 1
	fi
	nbTest=$(( $nbTest + 1 ))
}

echo
echo ELEKTRA SCRIPTS TESTS
echo

echo "testing set and get keys"

kdb -m "775" set $ROOT
exit_if_fail "could not set root"

[ -d "$DIR" ]
succeed_if "Directory not existing"

kdb set "$ROOT/value" "$VALUE"
exit_if_fail "could not set value"

[ -f "$FILE" ]
succeed_if "File not existing"

#echo "testing ls subcommand"

#[ "x`kdb ls $ROOT 2> /dev/null`" = "x$ROOT/value" ]
#succeed_if "cant ls $ROOT (may mean that $ROOT folder is not clean)"

#[ "x`kdb get $ROOT/value 2> /dev/null`" = "x$VALUE" ]
#exit_if_fail "cant get $ROOT/value"

#[ "x`kdb get -f $ROOT/value 2> /dev/null`" = "x$VALUE" ]
#succeed_if "-f option"

#[ "x`kdb get -d $ROOT/value 2> /dev/null`" = "x$ROOT/value=$VALUE" ]
#succeed_if "-d option"

#[ "x`kdb get -l $ROOT/value 2> /dev/null`" = "x$ROOT/value=$VALUE" ]
#succeed_if "-l option"

#[ "x`kdb get -s $ROOT/value 2> /dev/null`" = "xvalue=\"$VALUE\"" ]
#succeed_if "-s option"

kdb rm $ROOT/value
succeed_if "can remove user/test/value"

[ ! -f "$FILE" ]
succeed_if "File yet existing after delete"

kdb rm $ROOT

[ ! -d "$DIR" ]
succeed_if "Directory yet existing after delete"


#############
# xml tests
#############

echo "testing xml importing and exporting"

kdb import fstab.xml
succeed_if "importing fstab.xml failed"

kdb export user/tests/fstab > fstab-gen.xml
succeed_if "exporting user/tests/fstab failed"

diff fstab-gen.xml fstab-cmp.xml
succeed_if "xml files are not the same"

rm fstab-gen.xml
succeed_if "could not rm key-gen.xml"

kdb rm -r user/tests/fstab

echo test_script.sh RESULTS: $nbTest "test(s)" done $nbError "error(s)".
#exit $nbError

