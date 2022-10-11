@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK RESOLVER
echo

# Adds the specified namespace to a key name if the key does not have one.
# If the key already has a namespace, the namespace remains unchanged.
# The result is printed to stdout.
# $1 ... namespace
# $2 ... key name
prepend_namespace_if_not_present() {
	local present_namespace=$("$KDB" namespace $2)

	if [ -z "$present_namespace" ]; then
		echo $1:$2
	else
		echo $2
	fi
}

#set tmp path (mainly for macOS compatibility)
TMPPATH=$(
	cd /tmp
	pwd -P
)

#checks if resolver can be mounted and also partly checks if it resolves
#correctly (more tests welcome).

check_version

if is_plugin_available dump && is_plugin_available sync; then
	echo "dump and sync available"
else
	echo "dump or sync not available, skipping tests"
	nbSkip=$((nbSkip + 100))
	exit 0
fi

#enable with care: might remove more (empty) directories than it created
#root access required, writes into various paths (they are listed,
#even if WRITE_TO_SYSTEM is deactivated)
#WRITE_TO_SYSTEM=YES
WRITE_TO_SYSTEM=NO

ROOT_MOUNTPOINT=/test/script

#method that does all the checking
check_resolver() {
	if [ "$1" = "user" ]; then
		PLUGIN=$(echo "$PLUGINS_NEWLINES" | grep -m 1 "resolver_.*_$2.*_.*")
	else
		PLUGIN=$(echo "$PLUGINS_NEWLINES" | grep -m 1 "resolver_.*_.*_$2.*")
	fi

	if [ "$2" = "w" ]; then
		if is_plugin_available wresolver; then
			PLUGIN=wresolver
		else
			PLUGIN=""
		fi
	fi

	if [ -z "$PLUGIN" ]; then
		nbSkip=$((nbSkip + 1))
		echo "skipping test because plugin variant $2 is missing, for $2 with $3"
		return
	fi

	MOUNTPOINT=$1:$ROOT_MOUNTPOINT

	"$KDB" mount --resolver $PLUGIN "$3" $MOUNTPOINT dump 1> /dev/null
	succeed_if "could not mount root using: "$KDB" mount --resolver $PLUGIN $3 $MOUNTPOINT dump"

	FILE=$("$KDB" file -n $(prepend_namespace_if_not_present $1 $ROOT_MOUNTPOINT) 2> /dev/null)
	echo "For $1 $2 $3 we got $FILE"
	[ "x$FILE" = "x$4" ]
	succeed_if "resolving of $MOUNTPOINT did not yield $4 but $FILE"

	if [ "x$WRITE_TO_SYSTEM" = "xYES" ]; then
		KEY=$ROOT_MOUNTPOINT/key
		"$KDB" set $(prepend_namespace_if_not_present $1 $KEY) value
		succeed_if "could not set $KEY"

		echo "remove $FILE and its directories"
		rm $FILE
		succeed_if "could not remove $FILE"

		dirname $FILE
		rmdir -p --ignore-fail-on-non-empty $(dirname $FILE)
	fi

	"$KDB" umount $MOUNTPOINT > /dev/null
	succeed_if "could not umount $MOUNTPOINT"
}

# need HOME to work
unset USER

# HOME on buildserver wrong...
#check_resolver system b '~/FOO' ~/FOO
#check_resolver system b '~/x' ~/x
#check_resolver system b '~/x/y' ~/x/y
#check_resolver system b '~//x' ~//x
#check_resolver system b '~/' ~/
#check_resolver system b '~' ~/
#
#check_resolver spec b '~/x' ~/x
#check_resolver spec b '~/x/y' ~/x/y
#check_resolver spec b '~//x' ~//x
#check_resolver spec b '~/' ~/
#check_resolver spec b '~' ~/

unset HOME
unset USER

if echo "@KDB_DEFAULT_RESOLVER@" | grep "resolver_.*_.*_x.*"; then
	echo "skipping tests where XDG_CONFIG_DIRS is manipulated, because default resolver itself would use those paths"
	nbSkip=$((nbSkip + 10))
else

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

	export XDG_CONFIG_HOME="broken"
	check_resolver system x app/config_file /etc/xdg/app/config_file
	export XDG_CONFIG_HOME="(broken)"
	check_resolver system x app/config_file /etc/xdg/app/config_file
	export XDG_CONFIG_HOME="(even):(more):(broken):"
	check_resolver system x app/config_file /etc/xdg/app/config_file
	export XDG_CONFIG_HOME=""
	check_resolver system x app/config_file /etc/xdg/app/config_file
	unset XDG_CONFIG_HOME
	check_resolver system x app/config_file /etc/xdg/app/config_file

	OD=$(pwd)
	cd $TMPPATH # hopefully no @KDB_DB_DIR@ is in $TMPPATH
	check_resolver dir x /a $TMPPATH/a
	check_resolver dir x /a/b $TMPPATH/a/b
	check_resolver dir x a "$TMPPATH/@KDB_DB_DIR@/a"
	check_resolver dir x a/b "$TMPPATH/@KDB_DB_DIR@/a/b"
	cd "$OD"

fi # end of XDG tests

# FIXME [new_backend]: tests disabled, wresolver not working properly
# export ALLUSERSPROFILE="/C"
# check_resolver spec w /app/config_file /C/app/config_file
# check_resolver spec w app/config_file "/C@KDB_DB_SPEC@/app/config_file"
# check_resolver system w /app/config_file /C/app/config_file
# check_resolver system w app/config_file "/C@KDB_DB_SYSTEM@/app/config_file"
# unset ALLUSERSPROFILE
#
# export HOME="/D"
# check_resolver user w /app/config_file /D//app/config_file
# check_resolver user w app/config_file /D/app/config_file #@KDB_DB_USER@ not impl
# unset HOME
#
# OD="$(pwd)"
# cd $TMPPATH # hopefully no @KDB_DB_DIR@ is in $TMPPATH
# check_resolver dir w /a $TMPPATH//a
# check_resolver dir w /a/b $TMPPATH//a/b
# check_resolver dir w a $TMPPATH/a     #@KDB_DB_DIR@ not impl
# check_resolver dir w a/b $TMPPATH/a/b #@KDB_DB_DIR@ not impl
# cd "$OD"

# resolve ~ in paths
SYSTEM_DIR="$(echo @KDB_DB_SYSTEM@)"
SPEC_DIR="$(echo @KDB_DB_SPEC@)"

check_resolver system b x "$SYSTEM_DIR/x"
check_resolver system b x/a "$SYSTEM_DIR/x/a"
check_resolver system b /a /a
check_resolver system b /a/b/c /a/b/c

check_resolver spec b x "$SPEC_DIR/x"
check_resolver spec b x/a "$SPEC_DIR/x/a"
check_resolver spec b /x /x
check_resolver spec b /x/a /x/a

check_resolver user b x "@KDB_DB_HOME@/@KDB_DB_USER@/x"
check_resolver user b x/a "@KDB_DB_HOME@/@KDB_DB_USER@/x/a"
check_resolver user b /a "@KDB_DB_HOME@/a"

# empty env must have no influence
export HOME=""
export USER=""

check_resolver system b x "$SYSTEM_DIR/x"
check_resolver system b x/a "$SYSTEM_DIR/x/a"
check_resolver system b /a /a
check_resolver system b /a/b/c /a/b/c

check_resolver spec b x "$SPEC_DIR/x"
check_resolver spec b x/a "$SPEC_DIR/x/a"
check_resolver spec b /x /x
check_resolver spec b /x/a /x/a

check_resolver user b x "@KDB_DB_HOME@/@KDB_DB_USER@/x"
check_resolver user b x/a "@KDB_DB_HOME@/@KDB_DB_USER@/x/a"
check_resolver user b /a "@KDB_DB_HOME@/a"

OD=$(pwd)
cd $TMPPATH # hopefully no @KDB_DB_DIR@ is in $TMPPATH
check_resolver dir b /a "$TMPPATH/a"
check_resolver dir b /a/b "$TMPPATH/a/b"
check_resolver dir b a "$TMPPATH/@KDB_DB_DIR@/a"
check_resolver dir b a/b "$TMPPATH/@KDB_DB_DIR@/a/b"

T="$(
	cd $(mktempdir_elektra)
	pwd -P
)"

cleanup() {
	rm -rf "$T"
}

cd $T
check_resolver dir b /a $T/a
check_resolver dir b /a/b $T/a/b
check_resolver dir b a "$T/@KDB_DB_DIR@/a"
check_resolver dir b a/b "$T/@KDB_DB_DIR@/a/b"

mkdir $T/sub
cd $T/sub
touch $T/a
check_resolver dir b /a $T/a
check_resolver dir b /a/b $T/sub/a/b
rm $T/a

mkdir "$T/@KDB_DB_DIR@"
touch "$T/@KDB_DB_DIR@/a"
check_resolver dir b a "$T/@KDB_DB_DIR@/a"
check_resolver dir b a/b "$T/sub/@KDB_DB_DIR@/a/b"
rm "$T/@KDB_DB_DIR@/a"

cd "$OD"

unset HOME
unset USER

export HOME=/nowhere/below

check_resolver user h x "$HOME/@KDB_DB_USER@/x"

unset HOME
export USER=markus/somewhere/test

check_resolver user u abc "@KDB_DB_HOME@/$USER/@KDB_DB_USER@/abc"

end_script resolver
