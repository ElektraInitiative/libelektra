#!/bin/sh
#
# @brief Update Aptly DEB repository with new packages
#
# Moves new packages from an "incoming" directory to the local repository.
# Packages that are older than the version of the packages added (including revision) will be removed
# from the local repository.
# Publishes the local repository that was modified and signs it with gpg key.

set -ex

# Arguments
REPO_NAME=$1
REPO_PREFIX=$2
RELEASE_VERSION=$3
PACKAGE_REVISION=$4
# Constants
GPG_KEY="A9A25CC1CC83E839"
KEYRING="/home/jenkins/.gnupg/pubring.kbx"
PASSPHRASE_FILE="/home/jenkins/.aptly/secret"

REPO_SHOW_RET_VAL=0
aptly repo show "$REPO_NAME" > /dev/null 2>&1 || REPO_SHOW_RET_VAL=$?
if [ $REPO_SHOW_RET_VAL -ne 0 ]; then
	# Repository does not exist and needs to be created
	echo "Creating repository with name $REPO_NAME and publishing it under $REPO_PREFIX/$REPO_NAME"
	aptly repo create -architectures="amd64" -component=main -distribution="$REPO_NAME" "$REPO_NAME"
	aptly publish repo -gpg-key="$GPG_KEY" -keyring="$KEYRING" -batch=true --passphrase-file="$PASSPHRASE_FILE" "$REPO_NAME" "$REPO_PREFIX"
fi

aptly repo add -remove-files -force-replace "$REPO_NAME" /srv/libelektra/packaging/incoming/"$REPO_NAME"/*.d*eb
aptly repo remove "$REPO_NAME" "\$Version (<< $RELEASE_VERSION-$PACKAGE_REVISION)"
aptly repo show "$REPO_NAME"
aptly publish update -gpg-key="$GPG_KEY" -keyring="$KEYRING" -batch=true --passphrase-file="$PASSPHRASE_FILE" "$REPO_NAME" "$REPO_PREFIX"
