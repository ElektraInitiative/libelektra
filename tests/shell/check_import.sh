@INCLUDE_COMMON@

echo
echo ELEKTRA IMPORT SCRIPTS TESTS
echo

check_version


ROOT=$USER_ROOT
FILE=`mktemp`
SIDE=$ROOT/../side_val

cleanup()
{
	rm -f $FILE
}

[ -e /dev/stdin ]
exit_if_fail "For export/import /dev (and /proc) must be mounted"

[ -e /proc/self/fd/1 ]
exit_if_fail "For export/import /proc (and /dev) must be mounted"

for PLUGIN in $PLUGINS
do
	if is_not_rw_storage
	then
		echo "-- $PLUGIN not a read-write storage"
		continue;
	fi

	echo -------- $PLUGIN -----------

	echo "Import with existing root"

	$KDB set $ROOT "root" >/dev/null
	exit_if_fail "could not set root"

	test `$KDB ls $ROOT` = $ROOT
	succeed_if "Root key not found"

	$KDB import $ROOT $PLUGIN < $DATADIR/one_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script"
	succeed_if "key name not correct one_value"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"

	$KDB rm -r $ROOT
	succeed_if "Could not remove root"



	echo "Import with empty root"

	$KDB import $ROOT $PLUGIN < $DATADIR/one_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script"
	succeed_if "key name not correct one_value empty root"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"



	echo "Import with wrong root (overwrite)"

	$KDB set $SIDE val
	succeed_if "Could not set $SIDE"

	$KDB set $ROOT "wrong_root" >/dev/null
	exit_if_fail "could not set wrong_root"

	$KDB import -s overwrite $ROOT $PLUGIN < $DATADIR/one_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script"
	succeed_if "key name not correct"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"

	$KDB rm -r $ROOT
	succeed_if "Could not remove root"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $SIDE`" = val
		succeed_if "root value not correct"
	fi

	$KDB rm $SIDE
	succeed_if "Could not remove $SIDE"





	echo "Import two values"

	$KDB import $ROOT $PLUGIN < $DATADIR/two_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script
user/tests/script/key"
	succeed_if "key name not correct"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	test "`$KDB get $ROOT/key`" = value
	succeed_if "key value not correct"

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/two_value.$PLUGIN $FILE
	succeed_if "Export file two_value.$PLUGIN was not equal"



	echo "Import one value (cut two values from previous test case)"

	$KDB set $SIDE val
	succeed_if "Could not set $SIDE"

	$KDB import -s cut $ROOT $PLUGIN < $DATADIR/one_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script"
	succeed_if "key name not correct"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"

	test "`$KDB get $SIDE`" = val
	succeed_if "side value not correct"

	$KDB rm $SIDE
	succeed_if "Could not remove $SIDE"





	echo "Import one value (cut previous value)"

	$KDB set $ROOT wrong_root
	succeed_if "Could not set $ROOT"

	$KDB set $ROOT/val wrong_val
	succeed_if "Could not set $ROOT/val"

	$KDB set $SIDE val
	succeed_if "Could not set $SIDE"

	$KDB import -s cut $ROOT $PLUGIN < $DATADIR/one_value.$PLUGIN
	succeed_if "Could not run kdb import"

	test "`$KDB ls $ROOT`" = "user/tests/script"
	succeed_if "key name not correct"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $ROOT`" = root
		succeed_if "root value not correct"
	fi

	$KDB export $ROOT $PLUGIN > $FILE
	succeed_if "Could not run kdb export"

	diff $DATADIR/one_value.$PLUGIN $FILE
	succeed_if "Export file one_value.$PLUGIN was not equal"

	if [ "x$PLUGIN" != "xyajl" ]
	then
		#TODO: yajl currently cannot hold values within
		#directories, do not hardcode that
		test "`$KDB get $SIDE`" = val
		succeed_if "root value not correct"
	fi

	$KDB rm $SIDE
	succeed_if "Could not remove $SIDE"


	$KDB rm -r $ROOT
	succeed_if "Could not remove $ROOT"
done

end_script
