#!/bin/sh

set -ex

REPO_NAME=$1
REPO_PREFIX=$2
RELEASE_VERSION=$3
PACKAGE_REVISION=$4

aptly repo add -remove-files -force-replace $REPO_NAME /srv/libelektra/packaging/incoming/$REPO_NAME/*.d*eb
aptly repo remove $REPO_NAME "\$Version (<< $RELEASE_VERSION-$PACKAGE_REVISION)"
aptly repo show $REPO_NAME
aptly publish update -gpg-key="A9A25CC1CC83E839" -keyring=/home/jenkins/.gnupg/pubring.kbx -batch=true --passphrase-file="/home/jenkins/.aptly/secret" $REPO_NAME $REPO_PREFIX
