@INCLUDE_COMMON@

echo
echo ELEKTRA BASIC COMMAND SCRIPTS TESTS
echo

check_version

FILE=test_$RANDOMNAME.conf
VALUE=value

echo "testing set and get commands"

for PLUGIN in $PLUGINS
do
	echo -------- $PLUGIN -----------
	if [ ! "x`$KDB info $PLUGIN provides`" = "xstorage" ]
	then
		echo "$PLUGIN not a storage"
		continue;
	fi

	if [ "x$PLUGIN" = "xhosts" ]
	then
		echo "fstab does not work"
		continue;
	fi

	if [ "x$PLUGIN" = "xfstab" ]
	then
		echo "fstab does not work"
		continue;
	fi

	if [ "x$PLUGIN" = "xuname" ]
	then
		echo "uname cannot work"
		continue;
	fi

	if [ "x$PLUGIN" = "xyajl" ]
	then
		echo "yajl currently broken (to be fixed)"
		echo "TODO problem: empty file not empty (___empty_map)"
		continue;
	fi

	if [ "x$PLUGIN" = "xtcl" ]
	then
		PLUGIN="tcl ccode null"
		echo "tcl currently broken (to be fixed)"
		echo "TODO problem: after removing root key file is broken"
		continue;
	fi

	if [ "x$PLUGIN" = "xsimpleini" ]
	then
		PLUGIN="simpleini ccode null"
	fi

	unset -f cleanup

	$KDB mount $FILE $MOUNTPOINT $PLUGIN 1>/dev/null 2>/dev/null
	exit_if_fail "could not mount $FILE at $MOUNTPOINT using $PLUGIN"

	cleanup()
	{
		$KDB umount $MOUNTNAME >/dev/null
		succeed_if "could not umount $MOUNTNAME"
		rm -f $USER_FOLDER/$FILE
		rm -f $SYSTEM_FOLDER/$FILE
		rm -f $USER_FOLDER/$FILE.lck
		rm -f $SYSTEM_FOLDER/$FILE.lck
		rm -f $USER_FOLDER/$FILE.tmp
		rm -f $SYSTEM_FOLDER/$FILE.tmp
	}

	for ROOT in $USER_ROOT $SYSTEM_ROOT
	do
		echo "do preparation for $PLUGIN in $ROOT"
		$KDB set $ROOT "root" 1>/dev/null 2>/dev/null
		succeed_if "could not set root"

		[ "x`$KDB sget $ROOT/value defvalue 2> /dev/null`" = "xdefvalue" ]
		succeed_if "Did not get default value"

		[ "x`$KDB get $ROOT 2> /dev/null`" = "xroot" ]
		succeed_if "could not get root"

		[ "x`$KDB sget $ROOT default 2> /dev/null`" = "xroot" ]
		succeed_if "could not shell get root"

		$KDB set "$ROOT/value" "$VALUE" 1>/dev/null 2>/dev/null
		succeed_if "could not set value"

		[ "x`$KDB get $ROOT/value 2> /dev/null`" = "x$VALUE" ]
		succeed_if "cant get $ROOT/value"

		[ "x`$KDB sget $ROOT/value default 2> /dev/null`" = "x$VALUE" ]
		succeed_if "cant shell get $ROOT/value"

		echo "testing ls command"

		[ "x`$KDB ls $ROOT/value 2> /dev/null`" = "x$ROOT/value" ]
		succeed_if "cant ls $ROOT (may mean that $ROOT folder is not clean)"

		echo "testing rm command"

		$KDB rm $ROOT/value 1> /dev/null 2> /dev/null
		succeed_if "could not remove user/test/value"

		$KDB get $ROOT/value 1> /dev/null  2>/dev/null
		[ $? != "0" ]
		succeed_if "got removed key $ROOT/value"

		$KDB rm $ROOT 1>/dev/null 2>/dev/null
		succeed_if "could not remove user/test/value"

		[ "x`$KDB sget $ROOT/value value 2> /dev/null`" = "xvalue" ]
		succeed_if "Did not get default value"

		$KDB get $ROOT/value 1>/dev/null  2> /dev/null
		[ $? != "0" ]
		succeed_if "got removed key $ROOT"
	done

	cleanup
done

end_script basic commands
