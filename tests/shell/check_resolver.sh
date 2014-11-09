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

#enable with care: might remove more (empty) directories than it created
#root access required, writes into various pathes (they are listed,
#even if WRITE_TO_SYSTEM is deactivated)
#WRITE_TO_SYSTEM=YES

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

	$KDB mount --resolver $PLUGIN $3 $ROOT_MOUNTPOINT dump 1>/dev/null
	succeed_if "could not mount root: $3 at $ROOT_MOUNTPOINT with resolver $PLUGIN"

	FILE=`$KDB file -n $1$ROOT_MOUNTPOINT`
	[ "x$FILE"  = "x$4" ]
	succeed_if "resolving of user$ROOT_MOUNTPOINT did not yield $4"
	echo "got $FILE"

	if [ "x$WRITE_TO_SYSTEM" = "xYES" ]; then
		KEY=$1$ROOT_MOUNTPOINT/key
		$KDB set $KEY value
		succeed_if "could not set $KEY"

		echo "remove $FILE and its directories"
		rm $FILE
		succeed_if "could not remove $FILE"

		dirname $FILE
		rmdir -p --ignore-fail-on-non-empty `dirname $FILE`
	fi

	$KDB umount $ROOT_MOUNTNAME >/dev/null
	succeed_if "could not umount $ROOT_MOUNTNAME"
}

unset HOME
unset USER
unset XDG_CONFIG_DIRS
unset XDG_CONFIG_HOME

check_resolver system x app/config_file /etc/xdg/app/config_file

export XDG_CONFIG_DIRS="/xdg_dir"

check_resolver system x app/config_file /xdg_dir/app/config_file

export XDG_CONFIG_DIRS="/xdg_dir1:/xdg_dir2:/xdg_dir3"

check_resolver system x app/config_file /xdg_dir3/app/config_file

unset XDG_CONFIG_DIRS
export XDG_CONFIG_HOME="/xdg_dir1"

check_resolver system x app/config_file /etc/xdg/app/config_file
check_resolver user x app/config_file /xdg_dir1/app/config_file

unset XDG_CONFIG_HOME

check_resolver system b x @KDB_DB_SYSTEM@/x
check_resolver system b x/a @KDB_DB_SYSTEM@/x/a
check_resolver system b /a /a
check_resolver system b /a/b/c /a/b/c

check_resolver user b x @KDB_DB_HOME@/@KDB_DB_USER@/x
check_resolver user b x/a @KDB_DB_HOME@/@KDB_DB_USER@/x/a
check_resolver user b /a @KDB_DB_HOME@/a

# empty env must have no influence
export HOME=""
export USER=""
export XDG_CONFIG_HOME=""
export XDG_CONFIG_DIRS=""

check_resolver system b x @KDB_DB_SYSTEM@/x
check_resolver system b x/a @KDB_DB_SYSTEM@/x/a
check_resolver system b /a /a
check_resolver system b /a/b/c /a/b/c

check_resolver user b x @KDB_DB_HOME@/@KDB_DB_USER@/x
check_resolver user b x/a @KDB_DB_HOME@/@KDB_DB_USER@/x/a
check_resolver user b /a @KDB_DB_HOME@/a

unset HOME
unset USER
unset XDG_CONFIG_DIRS
unset XDG_CONFIG_HOME

export HOME=/nowhere/below

check_resolver user h x $HOME/@KDB_DB_USER@/x

unset HOME
export USER=markus/somewhere/test

check_resolver user u abc @KDB_DB_HOME@/$USER/@KDB_DB_USER@/abc

end_script resolver
