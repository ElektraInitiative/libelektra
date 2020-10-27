#!/bin/sh

BASE_DIR="$(pwd)"
SRC_DIR="$BASE_DIR/libelektra"
BUILD_DIR="$SRC_DIR/build"
FTP_DIR="$BASE_DIR/ftp"
DOC_DIR="$BASE_DIR/docu"

# quit with error if any command fails
#set -ex
set -x

install_elektra() {
	echo "Installing elektra..."
	rm -rf $BUILD_DIR
	mkdir $BUILD_DIR
	cd $BUILD_DIR
	cmake -DBUILD_SHARED=ON -DBUILD_FULL=ON -DBUILD_STATIC=ON -DKDB_DB_SYSTEM="${WORKSPACE}/config/kdb/system" -DKDB_DB_SPEC="${WORKSPACE}/config/kdb/spec" -DKDB_DB_HOME="${WORKSPACE}/config/kdb/home" -DCMAKE_INSTALL_PREFIX="${WORKSPACE}/system" ..
	make
	make install
	export LD_LIBRARY_PATH=${WORKSPACE}/system/lib:$LD_LIBRARY_PATH
	export PATH=${WORKSPACE}/system/bin:$PATH
	export DBUS_SESSION_BUS_ADDRESS=$(dbus-daemon --session --fork --print-address)
	export LUA_CPATH="${WORKSPACE}/system/lib/lua/5.2/?.so;"
	export VERSION=$(kdb get system/elektra/version/constants/KDB_VERSION)
	export DVERSION=$VERSION-1
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

update_debian_version_number() {
	echo "Updating debian version..."

	cd $SRC_DIR
	git checkout -B temp
	git tag -f $VERSION
	git checkout -B debian origin/debian
	git merge --no-ff -m 'merge $VERSION' temp

	dch --newversion $DVERSION "New upstream version."
	git add debian/changelog
	git commit -m "Debian Package $DVERSION (UNRELEASED)"
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
	make run_memcheck

	# log tests
	run_log_tests "test-src"

	# trace kdb calls
	log_strace "strace-src"

	# Check which files changed
	cd "$BUILD_DIR"
	DESTDIR=D make install
	cd $BUILD_DIR/D && find . | sort > $BASE_DIR/"$VERSION"/installed_files

	ls -l ${WORKSPACE}/system/lib/libelektra*"$VERSION" > $BASE_DIR/"$VERSION"/size
	readelf -a ${WORKSPACE}/system/lib/libelektra-core.so > $BASE_DIR/"$VERSION"/readelf-core

}

prepare_package() {
	echo "Preparing package..."

	export KDB_VERSION=$(kdb get system/elektra/version/constants/KDB_VERSION)
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
	make run_memcheck

	# Now copy the packages to ftp:
	cp $BUILD_DIR/elektra-$VERSION.tar.gz* $FTP_DIR/releases
	cp $FTP_DIR/releases/elektra-$VERSION.tar.gz $BASE_DIR/$VERSION/
}

configure_debian_package() {
	echo "Configuring debian package..."

	# Build deb:
	cd $SRC_DIR

	# UNRELEASED -> unstable
	dch --release $DVERSION "New upstream version"
	git add debian/changelog
	git commit -m "Debian Package $DVERSION"

	cp $FTP_DIR/releases/elektra-$VERSION.tar.gz $BASE_DIR/elektra_$VERSION.orig.tar.gz

	git clean -fdx
	rm -rf $BUILD_DIR
	gbp buildpackage -sa # TODO: use gpg key to sign debian package

	# move and install
	cd $BASE_DIR
	mkdir -p $BASE_DIR/$VERSION/debian/$DVERSION
	mv elektra_$DVERSION* *$DVERSION*.deb elektra_$VERSION.orig.tar.gz $BASE_DIR/$VERSION/debian/$DVERSION/
	# sudo dpkg -i $BASE_DIR/$VERSION/debian/$DVERSION/*$(dpkg-architecture -qDEB_BUILD_ARCH).deb

	# kdb --version | tee ~elektra/$VERSION/debian/version

	# trace kdb calls
	# log_strace "strace-debian-package"

	# run and log tests
	# run_log_tests "test-debian-package"

}

# NOTE: generation of man pages will currently not work, since
#	the buster docker image does not have ronn installed
build_documentation() {
	echo "Configuring docu..."

	git clean -fdx
	git checkout master
	# Add the API docu:
	mkdir $DOC_DIR/api/$VERSION/
	mkdir $BUILD_DIR
	cd $BUILD_DIR
	cmake -DBUILD_PDF=ON ..
	rm -rf doc
	make html man
	cp -ra doc/html doc/latex doc/man $DOC_DIR/api/$VERSION/

	# Symlink current to latest version and add everything
	cd $DOC_DIR/api/
	rm current
	ln -s $VERSION current
	git add current $VERSION
	git commit -a -m "$VERSION Release"
}

log_strace() {
	CONTEXT=$1
	mkdir $BASE_DIR/$VERSION/$CONTEXT

	strace -o $BASE_DIR/$VERSION/$CONTEXT/mount.strace kdb mount file.ecf user/release_test
	strace -o $BASE_DIR/$VERSION/$CONTEXT/file.strace kdb file user/release_test/b
	strace -o $BASE_DIR/$VERSION/$CONTEXT/set.strace kdb set user/release_test/b
	strace -o $BASE_DIR/$VERSION/$CONTEXT/get.strace kdb get user/release_test/b
	strace -o $BASE_DIR/$VERSION/$CONTEXT/rm.strace kdb rm user/release_test/b
	strace -o $BASE_DIR/$VERSION/$CONTEXT/umount.strace kdb umount user/release_test
}

run_log_tests() {
	CONTEXT=$1
	mkdir $BASE_DIR/$VERSION/$CONTEXT

	KDB=kdb kdb run_all -v 2>&1 | tee $BASE_DIR/$VERSION/$CONTEXT/run_all
	check_test_amount $BASE_DIR/$VERSION/$CONTEXT/run_all

	KDB=kdb-full kdb-full run_all 2>&1 | tee $BASE_DIR/$VERSION/$CONTEXT/run_all_full
	check_test_amount $BASE_DIR/$VERSION/$CONTEXT/run_all_full

	KDB=kdb-static kdb-static run_all 2>&1 | tee $BASE_DIR/$VERSION/$CONTEXT/run_all_static
	check_test_amount $BASE_DIR/$VERSION/$CONTEXT/run_all_static
}

check_test_amount() {
	TEST_LOG_PATH=$1

	# Check if there are really >=241 or >=131 tests
	TEST_COUNT=$(tail -n 1 $TEST_LOG_PATH | awk '/^run_all RESULTS: / { print $3 }')
	if [ "$TEST_COUNT" -lt "131" ]; then
		printf >&2 "Only $TEST_COUNT test(s) run"
		exit 1
	fi
}

install_elektra
run_updates
update_debian_version_number
run_checks
prepare_package
configure_debian_package
tar -czvf release.tar.gz $BASE_DIR/$VERSION
# build_documentation
