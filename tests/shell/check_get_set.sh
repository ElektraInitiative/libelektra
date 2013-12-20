@INCLUDE_COMMON@

echo
echo ELEKTRA BASIC COMMAND SCRIPTS TESTS
echo

check_version

VALUE=value

echo "testing set and get commands"

#override for specific testing
#PLUGINS=yajl

for PLUGIN in $PLUGINS
do
	if is_not_rw_storage
	then
		echo "$PLUGIN not a read-write storage"
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
		MOUNT_PLUGIN="tcl ccode null"
		echo "tcl currently broken (to be fixed)"
		echo "TODO problem: after removing root key file is broken"
		continue;
	fi

	if [ "x$PLUGIN" = "xsimpleini" ]
	then
		MOUNT_PLUGIN="simpleini ccode null"
	else
		MOUNT_PLUGIN=$PLUGIN
	fi

	unset -f cleanup
	FILE=test.$PLUGIN

	USER_REMAINING="`find $USER_FOLDER -maxdepth 1 -name $FILE'*' -print -quit`"
	test -z "$USER_REMAINING"
	exit_if_fail "files $USER_REMAINING in $USER_FOLDER would be removed during tests, so test is aborted"

	SYSTEM_REMAINING="`find $SYSTEM_FOLDER -maxdepth 1 -name $FILE'*' -print -quit`"
	test -z "$SYSTEM_REMAINING"
	exit_if_fail "files $SYSTEM_REMAINING in $SYSTEM_FOLDER would be removed during tests, so test is aborted"

	$KDB mount $FILE $MOUNTPOINT $MOUNT_PLUGIN 1>/dev/null
	exit_if_fail "could not mount $FILE at $MOUNTPOINT using $MOUNT_PLUGIN"

	cleanup()
	{
		$KDB umount $MOUNTNAME >/dev/null
		succeed_if "could not umount $MOUNTNAME"
		rm -f $USER_FOLDER/$FILE
		rm -f $SYSTEM_FOLDER/$FILE

		USER_REMAINING="`find $USER_FOLDER -maxdepth 1 -name $FILE'*' -print -exec rm {} +`"
		test -z "$USER_REMAINING"
		succeed_if "found remaining files $USER_REMAINING in $USER_FOLDER"

		SYSTEM_REMAINING="`find $SYSTEM_FOLDER -maxdepth 1 -name $FILE'*' -print -exec rm {} +`"
		test -z "$SYSTEM_REMAINING"
		succeed_if "found remaining files $SYSTEM_REMAINING in $SYSTEM_FOLDER"
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

		echo "testing array"

		KEY=$ROOT/hello/a/key
		$KDB set "$KEY" "$VALUE" 1>/dev/null
		succeed_if "could not set key $KEY"

		[ "x`$KDB get $KEY`" = "x$VALUE" ]
		succeed_if "$KEY is not $VALUE"

		for i in `seq 1 9`
		do
			KEY="$ROOT/hello/a/array/#$i"
			VALUE="$i"

			$KDB set "$KEY" "$VALUE" 1>/dev/null
			succeed_if "could not set key $ROOT/hello/a/array/#0"

			[ "x`$KDB get $KEY`" = "x$VALUE" ]
			succeed_if "$KEY is not $VALUE"
		done
	done

	cleanup
done

end_script basic commands
