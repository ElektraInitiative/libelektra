@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK REAL WORLD SUITE
echo

check_version

FILE_SUFFIX=test_real_world

check_remaining_files $FILE_SUFFIX

ROOT_FILE=${FILE_SUFFIX}_root.ecf
ROOT_MOUNTPOINT=/test/script
ROOT_MOUNTNAME=_test_script
if is_plugin_available dump
then
	$KDB mount $ROOT_FILE $ROOT_MOUNTPOINT dump 1>/dev/null
	succeed_if "could not mount root: $ROOT_FILE at $ROOT_MOUNTPOINT"
fi


SYS_FILE=${FILE_SUFFIX}_sys.ni
SYS_MOUNTPOINT=/test/script/sys
SYS_MOUNTNAME=_test_script_sys
if is_plugin_available ni
then
	$KDB mount $SYS_FILE $SYS_MOUNTPOINT ni 1>/dev/null
	succeed_if "could not mount SYS: $SYS_FILE at $SYS_MOUNTPOINT"
fi


HOSTS_FILE=${FILE_SUFFIX}_hosts
HOSTS_MOUNTPOINT=/test/script/sys/hosts
HOSTS_MOUNTNAME=_test_script_sys_hosts
if is_plugin_available hosts
then
	$KDB mount $HOSTS_FILE $HOSTS_MOUNTPOINT hosts 1>/dev/null
	succeed_if "could not mount hosts: $HOSTS_FILE at $HOSTS_MOUNTPOINT/hosts"
fi


UNAME_FILE=${FILE_SUFFIX}_uname
UNAME_MOUNTPOINT=system/test/script/sys/uname
UNAME_MOUNTNAME=system_test_script_sys_uname
if is_plugin_available uname
then
	touch $SYSTEM_FOLDER/$UNAME_FILE
	$KDB mount $UNAME_FILE $UNAME_MOUNTPOINT uname
	succeed_if "could not mount uname: $UNAME_FILE at $UNAME_MOUNTPOINT"

	# following keys must exist:
	$KDB ls $UNAME_MOUNTPOINT | grep "system/test/script/sys/uname/machine"
	succeed_if "machine key missing"
	$KDB ls $UNAME_MOUNTPOINT | grep "system/test/script/sys/uname/nodename"
	succeed_if "nodename key missing"
	$KDB ls $UNAME_MOUNTPOINT | grep "system/test/script/sys/uname/release"
	succeed_if "release key missing"
	$KDB ls $UNAME_MOUNTPOINT | grep "system/test/script/sys/uname/sysname"
	succeed_if "sysname key missing"
	$KDB ls $UNAME_MOUNTPOINT | grep "system/test/script/sys/uname/version"
	succeed_if "version key missing"

	$KDB get "system/test/script/sys/uname/machine"
	succeed_if "could not get machine key"
fi

APPS_FILE=${FILE_SUFFIX}_apps.ini
APPS_MOUNTPOINT=/test/script/apps
APPS_MOUNTNAME=_test_script_apps
if is_plugin_available simpleini
then
	$KDB mount $APPS_FILE $APPS_MOUNTPOINT simpleini ccode null
	succeed_if "could not mount simpleini: $APPS_FILE at $APPS_MOUNTPOINT"
fi


DESKTOP_FILE=${FILE_SUFFIX}_desktop.ini
DESKTOP_MOUNTPOINT=/test/script/apps/desktop
DESKTOP_MOUNTNAME=_test_script_apps_desktop
if is_plugin_available yajl
then
	$KDB mount $DESKTOP_FILE $DESKTOP_MOUNTPOINT yajl
	succeed_if "could not mount DESKTOP: $DESKTOP_FILE at $DESKTOP_MOUNTPOINT"

	check_set_rm system/test/script/apps/desktop/x y
	check_set_rm user/test/script/apps/desktop/x y
fi









if is_plugin_available dump
then
	$KDB mount | grep "test_real_world_root.ecf on /test/script with name _test_script"
	succeed_if "mountpoint $ROOT_MOUNTPOINT missing"

	check_set_rm system/test/script/next/key value
	check_set_rm user/test/script/next/key value
fi

if is_plugin_available ni
then
	$KDB mount | grep "test_real_world_sys.ni on /test/script/sys with name _test_script_sys"
	succeed_if "mountpoint $SYS_MOUNTPOINT missing"

	check_set_rm system/test/script/sys/next/key value
	check_set_rm user/test/script/sys/next/key value

	check_set_mv_rm system/test/script/sys/next/key user/test/script/next/key myvalue
fi

if is_plugin_available hosts
then
	$KDB mount | grep "test_real_world_hosts on /test/script/sys/hosts with name _test_script_sys_hosts"
	succeed_if "mountpoint $HOSTS_MOUNTPOINT missing"

	check_set_rm system/test/script/sys/hosts/ipv4/localhost 127.0.0.1
	check_set_rm user/test/script/sys/hosts/ipv4/localhost 127.0.0.1

	check_set_rm system/test/script/sys/hosts/ipv6/localhost ::1
	check_set_rm user/test/script/sys/hosts/ipv6/localhost ::1

	check_set_mv_rm user/test/script/sys/hosts/ipv4/localhost system/test/script/sys/hosts/ipv4/localhost myvalue

	check_set_mv_rm user/test/script/sys/hosts/ipv4/localhost system/test/script/sys/next/key myvalue
fi

if is_plugin_available simpleini
then
	$KDB mount | grep "test_real_world_apps.ini on /test/script/apps with name _test_script_apps"
	succeed_if "mountpoint $APPS_MOUNTPOINT missing"

	check_set_rm system/test/script/apps/next/x y
	check_set_rm user/test/script/apps/next/x y
	check_set_mv_rm user/test/script/apps/next/x/x/y system/test/script/sys/hosts/ipv4/localhost myvalue

	check_set_rm system/test/script/apps/next/x/a/b y
	check_set_rm user/test/script/apps/next/x/x/y y
fi











if is_plugin_available yajl
then
	$KDB umount $DESKTOP_MOUNTNAME >/dev/null
	succeed_if "could not umount $DESKTOP_MOUNTNAME"
fi

if is_plugin_available simpleini
then
	$KDB umount $APPS_MOUNTNAME >/dev/null
	succeed_if "could not umount $APPS_MOUNTNAME"
fi

if is_plugin_available uname
then
	$KDB umount $UNAME_MOUNTNAME >/dev/null
	succeed_if "could not umount $UNAME_MOUNTNAME"
fi

if is_plugin_available hosts
then
	$KDB mount | grep "test_real_world_hosts on /test/script/sys/hosts with name _test_script_sys_hosts"
	succeed_if "mountpoint $HOSTS_MOUNTPOINT missing"

	$KDB umount $HOSTS_MOUNTNAME >/dev/null
	succeed_if "could not umount $HOSTS_MOUNTNAME"

	$KDB mount | grep "test_real_world_hosts on /test/script/sys/hosts with name _test_script_sys_hosts"
	[ "$!" != "0"  ]
	succeed_if "mountpoint $HOSTS_MOUNTPOINT still there"
fi

if is_plugin_available ni
then
	$KDB umount $SYS_MOUNTNAME >/dev/null
	succeed_if "could not umount $SYS_MOUNTNAME"
fi

if is_plugin_available dump
then
	$KDB umount $ROOT_MOUNTNAME >/dev/null
	succeed_if "could not umount $ROOT_MOUNTNAME"
fi

rm -f $USER_FOLDER/$FILE_SUFFIX*
rm -f $SYSTEM_FOLDER/$FILE_SUFFIX*


end_script basic commands
