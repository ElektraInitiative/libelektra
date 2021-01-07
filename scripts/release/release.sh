#!/bin/sh

# quit with error if any command fails
set -ex

BASE_DIR="$(pwd)"
SRC_DIR="$BASE_DIR/libelektra"
SCRIPTS_DIR="$SRC_DIR/scripts"
PACKAGING_DIR="$SCRIPTS_DIR/packaging"
BUILD_DIR="$SRC_DIR/build"

PACKAGE_REVISION=${1:-1}

install_elektra() {
	echo "Installing elektra..."
	rm -rf $BUILD_DIR
	mkdir $BUILD_DIR
	cd $BUILD_DIR
	cmake -DBUILD_SHARED=ON \
		-DBUILD_FULL=ON \
		-DBUILD_STATIC=ON \
		-DKDB_DB_SYSTEM="${WORKSPACE}/config/kdb/system" \
		-DKDB_DB_SPEC="${WORKSPACE}/config/kdb/spec" \
		-DKDB_DB_HOME="${WORKSPACE}/config/kdb/home" \
		-DCMAKE_INSTALL_PREFIX="${WORKSPACE}/system" \
		..
	make
	make install
	export LD_LIBRARY_PATH=${WORKSPACE}/system/lib:$LD_LIBRARY_PATH
	export PATH=${WORKSPACE}/system/bin:$PATH
	export DBUS_SESSION_BUS_ADDRESS=$(dbus-daemon --session --fork --print-address) || true
	export LUA_CPATH="${WORKSPACE}/system/lib/lua/5.2/?.so;"
	export VERSION=$(kdb get system:/elektra/version/constants/KDB_VERSION)
	export PVERSION=$VERSION-$PACKAGE_REVISION
}

run_updates() {
	echo "Running updates..."
	mkdir $BASE_DIR/$VERSION

	# regenerate dot of plugins
	$SRC_DIR/scripts/dev/draw-all-plugins 2> $BASE_DIR/$VERSION/draw-all-plugins.error > $BASE_DIR/$VERSION/draw-all-plugins
	# Add generated dot of plugins to git and commit
	git add $SRC_DIR/doc/images/plugins.*
	git commit -a -m "Regenerate dot of plugins for release ${VERSION}"

	# run link checker
	cd $BUILD_DIR
	$SRC_DIR/scripts/link-checker external-links.txt 2> $BASE_DIR/$VERSION/link-checker.error > $BASE_DIR/$VERSION/link-checker
}

update_debian_changelog() {
	echo "Updating debian changelog..."

	cd $PACKAGING_DIR
	dch --newversion $PVERSION "New upstream version."
	dch --release $PVERSION "New upstream version"
	git add debian/changelog
	git commit -m "Update debian/changelog for release $PVERSION"
}

update_fedora_changelog() {
	echo "Updating fedora changelog..."

	cd $PACKAGING_DIR/fedora
	./update-rpm-changelog.sh -v "$PVERSION"
	git add changelog
	git commit -m "Update fedora/changelog for release $PVERSION"
}

run_checks() {
	echo "Running checks..."

	# check build-server
	kdb --version > $BASE_DIR/$VERSION/version

	# Rebuild cleanly, run all tests and also check for memleaks:
	cd "$BUILD_DIR"
	cmake -DPLUGINS="ALL" \
		-DTOOLS="ALL" \
		-DENABLE_DEBUG="OFF" \
		-DENABLE_LOGGER="OFF" \
		-DBUILD_SHARED=ON \
		-DBUILD_FULL=ON \
		-DBUILD_STATIC=ON \
		-DKDB_DB_SYSTEM="${WORKSPACE}/config/kdb/system" \
		-DKDB_DB_SPEC="${WORKSPACE}/config/kdb/spec" \
		-DKDB_DB_HOME="${WORKSPACE}/config/kdb/home" \
		-DCMAKE_INSTALL_PREFIX="${WORKSPACE}/system" \
		..
	make
	make run_all
	# memleak because of external tools on debian buster
	# discussed in https://github.com/ElektraInitiative/libelektra/pull/3530
	make run_memcheck || true

	$SCRIPTS_DIR/release/release-tests.sh $BASE_DIR $VERSION "src"

	# Check which files changed
	cd "$BUILD_DIR"
	DESTDIR=D make install
	DESTDIR_DEPTH=$(printf $BUILD_DIR/D | awk -F"/" '{print NF-1}')
	cd $BUILD_DIR/D && find . | cut -sd / -f $DESTDIR_DEPTH- | sort > $BASE_DIR/"$VERSION"/installed_files

	# get size of libs
	cd ${WORKSPACE}/system/lib/
	ls -l libelektra*"$VERSION" > $BASE_DIR/"$VERSION"/size

	# readelf of all libs
	mkdir $BASE_DIR/"$VERSION"/readelf
	for file in *.so; do
		readelf -a "$file" > $BASE_DIR/"$VERSION"/readelf/readelf-"$file"
	done

}

prepare_package() {
	echo "Preparing package..."

	export KDB_VERSION=$(kdb get system:/elektra/version/constants/KDB_VERSION)
	export CMAKE_BINARY_DIR=$BUILD_DIR

	cd $BUILD_DIR
	make source-package

	# Check if tar is reproduceable + sign it:
	gpg --sign elektra-$VERSION.tar.gz

	# Unpack + compile (with all available plugins) + test those sources:
	tar xvzf elektra-$VERSION.tar.gz
	mkdir $BUILD_DIR/builder
	cd $BUILD_DIR/builder
	cmake -DPLUGINS="ALL" \
		-DTOOLS="ALL" \
		-DENABLE_DEBUG="OFF" \
		-DENABLE_LOGGER="OFF" \
		-DBUILD_SHARED=ON \
		-DBUILD_FULL=ON \
		-DBUILD_STATIC=ON \
		-DKDB_DB_SYSTEM="${WORKSPACE}/config/kdb/system" \
		-DKDB_DB_SPEC="${WORKSPACE}/config/kdb/spec" \
		-DKDB_DB_HOME="${WORKSPACE}/config/kdb/home" \
		-DCMAKE_INSTALL_PREFIX="${WORKSPACE}/system" \
		../elektra-$VERSION
	make
	make run_all
	# memleak because of external tools on debian buster
	# discussed in https://github.com/ElektraInitiative/libelektra/pull/3530
	make run_memcheck || true

	cp $BUILD_DIR/elektra-$VERSION.tar.gz* $BASE_DIR/$VERSION/
}

configure_package() {
	echo "Configuring debian package..."

	cd $SRC_DIR
	git clean -fdx
	mkdir $BUILD_DIR
	cd $BUILD_DIR

	# get version codename
	VERSION_CODENAME=$(grep "VERSION_CODENAME=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
	if [ -z ${VERSION_CODENAME} ]; then
		OS_ID=$(grep "^ID=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
		VERSION_ID=$(grep "VERSION_ID=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
		VERSION_CODENAME="$OS_ID$VERSION_ID"
	fi

	mkdir -p $BASE_DIR/$VERSION/$VERSION_CODENAME
	$SCRIPTS_DIR/packaging/package.sh "$PACKAGE_REVISION" 2> $BASE_DIR/$VERSION/$VERSION_CODENAME/elektra_$PVERSION.build.error > $BASE_DIR/$VERSION/$VERSION_CODENAME/elektra_$PVERSION.build # $DIST_NAME #TODO: sign package

	mv $BUILD_DIR/package/* $BASE_DIR/$VERSION/$VERSION_CODENAME/
}

install_elektra
run_updates
git tag -f $VERSION # needed by `make source-package
update_fedora_changelog
update_debian_changelog
run_checks
prepare_package
configure_package
cd $BASE_DIR
$SCRIPTS_DIR/release/sign-packages.sh $BASE_DIR/$VERSION/$VERSION_CODENAME
tar -czvf release.tar.gz ./$VERSION
