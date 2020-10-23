#!/bin/sh

SCRIPTS_DIR=$(dirname "$0")
BASE_DIR="$(pwd)"
SRC_DIR="$BASE_DIR/libelektra"
BUILD_DIR="$SRC_DIR/build"
FTP_DIR="$BASE_DIR/ftp"
DOC_DIR="$BASE_DIR/docu"

install_elektra() {
	echo "Installing elektra..."
	rm -rf $BUILD_DIR && mkdir $BUILD_DIR && cd $BUILD_DIR && cmake -DBUILD_SHARED=ON -DBUILD_FULL=ON -DBUILD_STATIC=ON ..
	cd ..
	cmake --build build -- -j5
	cd $BUILD_DIR
	make install
	export VERSION=$(kdb get system/elektra/version/constants/KDB_VERSION)
	export DVERSION=$VERSION-1
}

run_updates() {
	echo "Running updates..."
	mkdir $BASE_DIR/$VERSION
	# regenerate dot of plugins
	$SRC_DIR/scripts/dev/draw-all-plugins > $BASE_DIR/$VERSION/draw-all-plugins # TODO: rename file
	# run link checker
	cd $BUILD_DIR && $SRC_DIR/scripts/link-checker external-links.txt > $BASE_DIR/$VERSION/link-checker # TODO: rename file
}

update_debian_version_number() {
	echo "Updating debian version..."
	cd $SRC_DIR
	dch --newversion $DVERSION "New upstream version."
	git add debian/changelog
	git commit -m "Debian Package $DVERSION (UNRELEASED)"
}


cleanup() {
	echo "Running cleanup..."
	git checkout master

	# merge any left over feature branches
	git merge --no-ff feature -m "Merge feature branches for ${VERSION} release"
	git commit -a -m "Release preperation for ${VERSION}"
	# cleanup
	git clean -dfx

	# make an empty builddirectory:
	mkdir $BUILD_DIR
}

run_checks() {
	echo "Running checks..."

	# check build-server
	kdb --version > $BASE_DIR/$VERSION/version

	# Rebuild cleanly, run all tests and also check for memleaks:
	cd "$BUILD_DIR" && $SRC_DIR/scripts/dev/configure-debian "$SRC_DIR" && make -j24 && make run_all -j24 && make run_memcheck -j24

	# Check if there are really >=241 or >=131 tests
	TEST_COUNT=$(tail -n 1 run_all | awk '/^run_all RESULTS: / { print $3 }')
	if [ "$TEST_COUNT" -lt "131" ]; then
		printf >&2 "Only $TEST_COUNT test(s) run"
		exit 1
	fi

	# log tests
	run_log_tests "test-src"

	# Check which files changed
	DESTDIR=D make install -j5
	cd $BUILD_DIR/D && find . | sort > $BASE_DIR/"$VERSION"/installed_files

	log_strace "strace-src"

	ls -l /usr/local/lib/libelektra-core.so."$VERSION" > $BASE_DIR/"$VERSION"/size 
	readelf -a /usr/local/lib/libelektra-core.so > $BASE_DIR/"$VERSION"/readelf-core 

}

prepare_package() {
	echo "Preparing package..."

	export KDB_VERSION=$(kdb get system/elektra/version/constants/KDB_VERSION)
	export CMAKE_BINARY_DIR=$BUILD_DIR

	cd "$BUILD_DIR" && make source-package

	# Check if tar is reproduceable + sign it:
	gpg --sign elektra-$VERSION.tar.gz 

	# Unpack + compile (with all available plugins) + test those sources:
	tar xvzf elektra-$VERSION.tar.gz && mkdir builder
	cd $BUILD_DIR/builder && $SRC_DIR/scripts/dev/configure-debian ../elektra-$VERSION && make -j5 && make run_all && make run_memcheck

	# Now copy the packages to ftp:
	cp $BUILD_DIR/elektra-$VERSION.tar.gz* $FTP_DIR/releases
	cp $FTP_DIR/releases/elektra-$VERSION.tar.gz $BASE_DIR/$VERSION/
}

configure_debian_package() {
	echo "Configuring debian package..."

	# Build deb:
	cd $SRC_DIR && git checkout debian
	git merge --no-ff $VERSION -m "Merge for debian release $DVERSION"

	# UNRELEASED -> unstable
	dch --release $DVERSION "New upstream version"
	git add debian/changelog
	git commit -m "Debian Package $DVERSION"

	cp $FTP_DIR/releases/elektra-$VERSION.tar.gz $BASE_DIR/elektra_$VERSION.orig.tar.gz

	git clean -fdx && gpg --sign $(mktemp) # so that the step afterwards won't abort
	gbp buildpackage -rfakeroot -sa

	# move and install
	cd $BASE_DIR && mkdir -p $BASE_DIR/$VERSION/debian/$DVERSION
	mv elektra_$DVERSION* *$DVERSION*.deb elektra_$VERSION.orig.tar.gz $BASE_DIR/$VERSION/debian/$DVERSION/
	sudo dpkg -i $BASE_DIR/$VERSION/debian/$DVERSION/*$(dpkg-architecture -qDEB_BUILD_ARCH).deb

	# trace kdb calls
	log_strace "strace-debian-package"

	# run and log tests
	run_log_tests "test-debian-package"

}

configure_docu() {
	echo "Configuring docu..."
	# Add the API docu:
	mkdir $DOC_DIR/api/$VERSION/
	cd $BUILD_DIR && cmake -DBUILD_PDF=ON . && rm -rf doc && make html man && cp -ra doc/html doc/latex doc/man $DOC_DIR/api/$VERSION/

	# Symlink current to latest version and add everything:
	cd $DOC_DIR/api/ && rm current && ln -s $VERSION current && git add current $VERSION && git commit -a -m "$VERSION Release"
}

log_strace() {
	mkdir $BASE_DIR/$VERSION/$1

	strace -o $BASE_DIR/$VERSION/$1/mount.strace kdb mount file.ecf user/release_test
	strace -o $BASE_DIR/$VERSION/$1/file.strace kdb file user/release_test/b
	strace -o $BASE_DIR/$VERSION/$1/set.strace kdb set user/release_test/b
	strace -o $BASE_DIR/$VERSION/$1/get.strace kdb get user/release_test/b
	strace -o $BASE_DIR/$VERSION/$1/rm.strace kdb rm user/release_test/b
	strace -o $BASE_DIR/$VERSION/$1/umount.strace kdb umount user/release_test
}

run_log_tests() {
	mkdir $BASE_DIR/$VERSION/$1
	KDB=kdb kdb run_all -v | tee $BASE_DIR/$VERSION/$1/run_all 2>&1
	KDB=kdb-full kdb-full run_all | tee $BASE_DIR/$VERSION/$1/run_all_full 2>&1

}

install_elektra
run_updates
update_debian_version_number
cleanup
run_checks
prepare_package
configure_debian_package
tar -czvf release.tar.gz $BASE_DIR/$VERSION
configure_docu
