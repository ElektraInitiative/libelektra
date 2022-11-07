#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK MOUNT JAVA
echo

if ! command -v java > /dev/null; then
	echo "No Java installation found."
	exit 0
fi

"$KDB" mount-java mountjava.cfg user:/test/mountjava dump java:org.libelektra.plugin.WhitelistPlugin kdb:type
succeed_if "mount-java not successful"

"$KDB" umount user:/test/mountjava

end_script mount_java
