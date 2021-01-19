#!/bin/sh
#
# @brief Sign packages

set -ex

DESTINATION=$1

files_with_extension_exists() {
	FILE_EXTENSION=$1

	count=$(ls -1 "$DESTINATION"/*."$FILE_EXTENSION" 2> /dev/null | wc -l)
	if [ "$count" != 0 ]; then
		return 0
	else
		return 1
	fi
}

if files_with_extension_exists "deb"; then
	echo "Signing Debian packages ..."
	for package in $DESTINATION/*.d*eb; do
		debsigs --sign=origin "$package"
	done
elif files_with_extension_exists "rpm"; then
	echo "Skipping RPM package signing ..."
	# is currently in Jenkins not possible because importing a gpg key into rpm
	# requires root privileges
else
	echo "No supported packages found."
	exit 1
fi
