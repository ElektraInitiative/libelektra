#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK ERROR
echo

check_version

FILE_SUFFIX=test_error

check_remaining_files $FILE_SUFFIX

ROOT=user:/test/script
ROOT_FILE=${FILE_SUFFIX}_root.ecf
ROOT_MOUNTPOINT=/test/script
if ! is_plugin_available dump || ! is_plugin_available sync; then
	echo "Need dump and sync to run test, will abort"
	exit 0
fi

"$KDB" mount $ROOT_FILE $ROOT_MOUNTPOINT dump > /dev/null 2>&1
succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT"

"$KDB" set $ROOT/valueable_data important_unrecoverable_data > /dev/null
succeed_if "cannot set valuable data"

"$KDB" meta set $ROOT/valueable_data trigger/error "C03100"
succeed_if "cannot set metadata"

TMPFILE="$(mktempfile_elektra)"
cleanup() {
	rm -f "$TMPFILE"
}

ERROR_FILE=${FILE_SUFFIX}_error.ecf
#subfolders not supported:
#USER_ERROR_FOLDER=${USER_FOLDER}/subfolder
#SYSTEM_ERROR_FOLDER=${SYSTEM_FOLDER}/subfolder
USER_ERROR_FILE=${USER_FOLDER}/${ERROR_FILE}
SYSTEM_ERROR_FILE=${SYSTEM_FOLDER}/${ERROR_FILE}
ERROR_MOUNTPOINT=/test/script/error
if is_plugin_available error; then
	echo "Testing operations on erroneous backends"

	"$KDB" mount $ERROR_FILE $ERROR_MOUNTPOINT dump error > /dev/null 2>&1
	succeed_if "could not mount error at $ERROR_MOUNTPOINT"

	"$KDB" mv $ROOT/valueable_data $ROOT/error/dump > "$TMPFILE" 2>&1
	[ $? -ne 0 ]
	succeed_if "Was able to move to error plugin"

	CONTENT=$(cat "$TMPFILE")

	grep "ERROR \[C03100\]" "$TMPFILE" > /dev/null
	succeed_if "Triggered error did not occur, got $CONTENT"

	grep "from error plugin" "$TMPFILE" > /dev/null
	succeed_if "Error does not stem from error plugin"

	[ "x$("$KDB" ls $ROOT 2> /dev/null)" = "x$ROOT/valueable_data" ]
	succeed_if "cant ls $ROOT (may mean that $ROOT folder is not clean)"

	[ "x$("$KDB" get $ROOT/valueable_data 2> /dev/null)" = "ximportant_unrecoverable_data" ]
	succeed_if "Important data lost"

	"$KDB" cp $ROOT/valueable_data $ROOT/error/dump > "$TMPFILE" 2>&1
	[ $? -ne 0 ]
	succeed_if "Was able to copy to error plugin"

	grep "ERROR \[C03100\]" "$TMPFILE" > /dev/null
	succeed_if "Triggered error did not occur, got $CONTENT"

	grep "from error plugin" "$TMPFILE" > /dev/null
	succeed_if "Error does not stem from error plugin, got $CONTENT"

	[ "x$("$KDB" ls $ROOT 2> /dev/null)" = "x$ROOT/valueable_data" ]
	succeed_if "cant ls $ROOT (may mean that $ROOT folder is not clean)"

	[ "x$("$KDB" get $ROOT/valueable_data 2> /dev/null)" = "ximportant_unrecoverable_data" ]
	succeed_if "data would have been lost"

	"$KDB" umount $ERROR_MOUNTPOINT > /dev/null
	succeed_if "could not umount $ERROR_MOUNTPOINT"

	#Excluded: a bit unclear which behavior is wanted
	#echo "Test error plugin when open"
	#
	#$KDB mount $ERROR_FILE $ERROR_MOUNTPOINT dump error on_open/error=10 > $TMPFILE 2>&1
	#[ $? -ne 0 ]
	#succeed_if "could mount error at $ERROR_MOUNTPOINT"
	#
	#CONTENT=`cat $TMPFILE`
	#
	#grep "[eE]rror (#C03100) occurred!" $TMPFILE > /dev/null
	#succeed_if "Error not found in output, got $CONTENT"
	#
	#$KDB get $ERROR_MOUNTPOINT > /dev/null 2>&1
	#[ $? -ne 0 ]
	#succeed_if "Was able to get from missing backend"
	#
	#$KDB get system:$ERROR_MOUNTPOINT > $TMPFILE 2>&1
	#[ $? -ne 0 ]
	#succeed_if "Was able to get from missing backend"
	#
	#CONTENT=`cat $TMPFILE`
	#
	#grep "[eE]rror (#10) occurred!" $TMPFILE > /dev/null
	#succeed_if "Error not found in output, got $CONTENT"
	#
	#
	#$KDB umount $ERROR_MOUNTPOINT >/dev/null
	#succeed_if "could not umount $ERROR_MOUNTPOINT"
fi

"$KDB" umount $ROOT_MOUNTPOINT > /dev/null
succeed_if "could not umount $ROOT_MOUNTPOINT"

rm -f "$TMPFILE"
rm -f "$USER_FOLDER"/$FILE_SUFFIX*
rm -f "$SYSTEM_FOLDER"/$FILE_SUFFIX*
#rmdir $USER_ERROR_FOLDER
#rmdir $SYSTEM_ERROR_FOLDER

end_script error
