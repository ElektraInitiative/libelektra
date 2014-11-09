@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK RESOLVER
echo

#checks if resolver can be mounted and also partly checks if it resolves
#correctly (more tests welcome).

check_version

if is_plugin_available dump
then
	echo "dump available"
else
	echo "dump not available, skipping tests"
	exit 0
fi

ROOT_FILE=resolver_check.ecf
ROOT_MOUNTPOINT=/test/script
ROOT_MOUNTNAME=_test_script

#method that does all the checking
check_resolver()
{
	if [ "$1" = "user" ]
	then
		PLUGIN=`echo "$PLUGINS_NEWLINES" | grep -m 1 "resolver_.*_$2.*_.*"`
	else
		PLUGIN=`echo "$PLUGINS_NEWLINES" | grep -m 1 "resolver_.*_.*_$2.*"`
	fi

	if [ -z "$PLUGIN" ]
	then
		echo "No plugin matching $2 for namespace $1"
		return
	fi

	$KDB mount --resolver $PLUGIN $ROOT_FILE $ROOT_MOUNTPOINT dump 1>/dev/null
	succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT with resolver $PLUGIN"

	RES=`USER=nobody $KDB file -n user$ROOT_MOUNTPOINT`
	[ "x$RES"  = "x$3" ]
	succeed_if resolving of user$ROOT_MOUNTPOINT did not yield $3 but $RES

	$KDB umount $ROOT_MOUNTNAME >/dev/null
	succeed_if "could not umount $ROOT_MOUNTNAME"
}

unset HOME
unset USER

check_resolver user b @KDB_DB_HOME@/@KDB_DB_USER@/$ROOT_FILE

export HOME=nowhere

check_resolver user h /nowhere/@KDB_DB_USER@/$ROOT_FILE

unset HOME
export USER=nobody

check_resolver user u @KDB_DB_HOME@/nobody/@KDB_DB_USER@/$ROOT_FILE

end_script resolver
