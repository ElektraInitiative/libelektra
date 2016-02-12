@INCLUDE_COMMON@

echo
echo ELEKTRA EXPORT SCRIPTS TESTS
echo

check_version

ROOT=$USER_ROOT
FILE="$(mktempfile_elektra)"
PLUGIN=$PLUGIN

cleanup()
{
	rm -f $FILE
}


[ -e /dev/stdout ]
exit_if_fail "For export/import /dev must be mounted"

[ -e /dev/stdin ]
exit_if_fail "For export/import /dev must be mounted"

for PLUGIN in $PLUGINS
do
	if is_not_rw_storage
	then
		echo "$PLUGIN not a read-write storage"
		continue;
	fi

	echo -------- $PLUGIN -----------

	if [ "x$PLUGIN" = "xini" ]
	then
		$KDB mount test.ini $ROOT $PLUGIN 
	fi

	$KDB set $ROOT "root" >/dev/null
	exit_if_fail "could not set root"

	test `$KDB ls $ROOT` = $ROOT
	succeed_if "Root key not found"

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"


	test "`$KDB set $ROOT/key "value"`" = "Create a new key $ROOT/key with string value"
	succeed_if "Could not set $ROOT/key"

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/two_value.$PLUGIN $FILE
	succeed_if "Export file two_value.$PLUGIN was not equal"


	$KDB set $ROOT/key/subkey "another value" > /dev/null
	succeed_if "Could not set $ROOT/key/subkey"

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/three_value.$PLUGIN $FILE
	succeed_if "Export file three_value.$PLUGIN was not equal"


	$KDB rm $ROOT/key > /dev/null
	succeed_if "Could not rm $ROOT/key"

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/again_two_value.$PLUGIN $FILE
	succeed_if "Export file again_two_value.$PLUGIN was not equal"


	$KDB rm -r $ROOT
	succeed_if "Could not remove root"

	if [ "x$PLUGIN" = "xini" ]
	then
		rm `$KDB file $ROOT`
		$KDB umount $ROOT
	fi


done

end_script
