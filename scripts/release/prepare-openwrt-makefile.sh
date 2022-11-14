#!/bin/sh
#
# @brief Removes variables of the OpenWrt makefile that are intended for
# 		 release builds and add configuration so the current state of
# 		 the master branch is used for building an OpenWrt package.

set -e

MAKEFILE_PATH=$1

# Remove variables used for release builds
sed -i '/^PKG_HASH:=/d' "$MAKEFILE_PATH"
sed -i '/^PKG_SOURCE:=/d' "$MAKEFILE_PATH"
sed -i '/^PKG_SOURCE_URL:=/d' "$MAKEFILE_PATH"

# Uncomment variables to build master branch
sed -i '/^#PKG_SOURCE_PROTO:=.*/s/^#//' "$MAKEFILE_PATH"
sed -i '/^#PKG_SOURCE_URL:=.*/s/^#//' "$MAKEFILE_PATH"
sed -i '/^#PKG_SOURCE_SUBDIR:=.*/s/^#//' "$MAKEFILE_PATH"
sed -i '/^#PKG_SOURCE:=.*/s/^#//' "$MAKEFILE_PATH"
# Set source version to master
sed -i 's/^#PKG_SOURCE_VERSION:=.*/PKG_SOURCE_VERSION:=master/g' "$MAKEFILE_PATH"
