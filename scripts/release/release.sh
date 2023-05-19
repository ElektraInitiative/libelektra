#!/bin/sh
#
# @brief Prepare for release, run tests and generate release artifacts

# quit with error if any command fails
set -ex

BASE_DIR="$(pwd)"
SRC_DIR="$BASE_DIR/libelektra"
SCRIPTS_DIR="$SRC_DIR/scripts"
PACKAGING_DIR="$SCRIPTS_DIR/packaging"
BUILD_DIR="$SRC_DIR/build"
PREVIOUS_RELEASE_LOGS="$BASE_DIR/prev-release-logs"

get_current_git_version_tag() {
	git tag -l --sort=version:refname | sed 's/v//' | grep -e '[0-9].[0-9].[0-9]*' | tail -n1
}
get_previous_git_version_tag() {
	git tag -l --sort=version:refname | sed 's/v//' | grep -e '[0-9].[0-9].[0-9]*' | tail -n2 | head -n1
}

cd "$SRC_DIR"

PACKAGE_REVISION=${1:-1}
PREVIOUS_RELEASE_VERSION=${2:-$(get_previous_git_version_tag)}

find_version_codename() {
	VERSION_CODENAME=$(grep "VERSION_CODENAME=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
	if [ -z "${VERSION_CODENAME}" ]; then
		OS_ID=$(grep "^ID=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
		VERSION_ID=$(grep "VERSION_ID=" /etc/os-release | awk -F= {' print $2'} | sed s/\"//g)
		if [ -z "${OS_ID}" ] || [ -z "${VERSION_ID}" ]; then
			VERSION_CODENAME=$(dpkg --status tzdata | grep Provides | cut -f2 -d'-')
		else
			VERSION_CODENAME="$OS_ID$VERSION_ID"
		fi
	fi

	echo "Version codename: ${VERSION_CODENAME}"
}

install_elektra() {
	echo "Installing elektra..."
	rm -rf "$BUILD_DIR"
	mkdir "$BUILD_DIR"
	cd "$BUILD_DIR"
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
	rm -rf "$BASE_DIR"/"$VERSION"
	mkdir "$BASE_DIR"/"$VERSION"

	# regenerate dot of plugins
	"$SRC_DIR"/scripts/dev/draw-all-plugins 2> "$BASE_DIR"/"$VERSION"/draw-all-plugins.error > "$BASE_DIR"/"$VERSION"/draw-all-plugins
	git add "$SRC_DIR"/doc/images/plugins.*
	git commit -a -m "release: regenerate plugins overview picture" || true

	# update info status
	# cd $SRC_DIR
	# $SCRIPTS_DIR/dev/update-infos-status 2> $BASE_DIR/$VERSION/update-infos-status.error > $BASE_DIR/$VERSION/update-infos-status
	# git commit -a -m "release: update plugin info status" || true

	# run link checker
	cd "$BUILD_DIR"
	"$SRC_DIR"/scripts/link-checker external-links.txt 2> "$BASE_DIR"/"$VERSION"/link-checker.error > "$BASE_DIR"/"$VERSION"/link-checker
}

git_tag() {
	cd "$SRC_DIR"
	PREVIOUS_RELEASE_TAG=$(get_current_git_version_tag | sed 's/v//')
	if [ "$PREVIOUS_RELEASE_TAG" != "$VERSION" ]; then
		git tag "v$VERSION" -m "Release v$VERSION" # needed by `make source-package` and `git-release-stats
	else
		echo "VERSION equals latest git version tag. Git tag will not be created."
	fi
}

update_debian_changelog() {
	echo "Updating debian changelog..."

	cd "$PACKAGING_DIR"
	dch --newversion "$PVERSION" "New upstream version."
	dch --release "$PVERSION" "New upstream version"
	git add debian/changelog
	git commit -m "release: update debian/changelog"
}

update_fedora_changelog() {
	echo "Updating fedora changelog..."

	cd "$PACKAGING_DIR"/fedora
	./update-rpm-changelog.sh -v "$PVERSION"
	git add changelog
	git commit -m "release: update fedora/changelog"
}

export_git_log() {
	cd "$SRC_DIR"
	GIT_LOG_DIR="$BASE_DIR/$VERSION/git"
	mkdir "$GIT_LOG_DIR"
	# export git diff since 1 day (changes done in pipeline)
	git log -p --since="1 days ago" > "$GIT_LOG_DIR/master.log"
	# get latest two version tags
	PREVIOUS_RELEASE=$(get_previous_git_version_tag)
	CURRENT_RELEASE=$(get_current_git_version_tag)
	# generate git statistics
	"$SCRIPTS_DIR"/git-release-stats "$PREVIOUS_RELEASE" "$CURRENT_RELEASE" > "$GIT_LOG_DIR/statistics"
}

run_checks() {
	echo "Running checks..."

	# check build-server
	kdb --version > "$BASE_DIR"/"$VERSION"/version

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
	memcheck

	"$SCRIPTS_DIR"/release/release-tests.sh "$BASE_DIR" "$VERSION" "src"

	# Check which files changed
	cd "$BUILD_DIR"
	DESTDIR=D make install
	DESTDIR_DEPTH=$(printf "$BUILD_DIR"/D | awk -F"/" '{print NF-1}')
	cd "$BUILD_DIR"/D && find . | cut -sd / -f "$DESTDIR_DEPTH"- | sort > "$BASE_DIR"/"$VERSION"/installed_files

	# create diff of installed files
	# diff returns 0 if no diff, 1 if diff and 2 on errors
	DIFF_RET_VAL=0
	diff "$BASE_DIR"/"$VERSION"/installed_files "$PREVIOUS_RELEASE_LOGS"/installed_files > "$BASE_DIR"/"$VERSION"/installed_files_diff || DIFF_RET_VAL=$?
	if [ "$DIFF_RET_VAL" -gt "1" ]; then
		echo "diff command returned status code $DIFF_RET_VAL"
		exit 1
	fi

	# get size of libs
	cd "${WORKSPACE}"/system/lib/
	ls -l libelektra*"$VERSION" > "$BASE_DIR"/"$VERSION"/size

	# readelf of all libs
	mkdir "$BASE_DIR"/"$VERSION"/readelf
	for file in *.so; do
		readelf -a "$file" > "$BASE_DIR"/"$VERSION"/readelf/readelf-"$file"
	done
}

create_source_package() {
	echo "Preparing package..."

	export KDB_VERSION=$(kdb get system:/elektra/version/constants/KDB_VERSION)
	export CMAKE_BINARY_DIR=$BUILD_DIR

	cd "$BUILD_DIR"
	make source-package

	# Check if tar is reproduceable + sign it:
	gpg --sign elektra-"$VERSION".tar.gz

	# Unpack + compile (with all available plugins) + test those sources:
	tar xvzf elektra-"$VERSION".tar.gz
	mkdir "$BUILD_DIR"/builder
	cd "$BUILD_DIR"/builder
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
		../elektra-"$VERSION"
	make
	make run_all
	memcheck

	cp "$BUILD_DIR"/elektra-"$VERSION".tar.gz* "$BASE_DIR"/"$VERSION"/
}

build_package() {
	echo "Building packages..."

	cd "$SRC_DIR"
	git clean -fdx
	mkdir "$BUILD_DIR"
	cd "$BUILD_DIR"

	mkdir -p "$BASE_DIR"/"$VERSION"/package/"$VERSION_CODENAME"
	"$SCRIPTS_DIR"/packaging/package.sh "$PACKAGE_REVISION" 2> "$BASE_DIR"/"$VERSION"/package/"$VERSION_CODENAME"/elektra_"$PVERSION".build.error > "$BASE_DIR"/"$VERSION"/package/"$VERSION_CODENAME"/elektra_"$PVERSION".build

	mv "$BUILD_DIR"/package/* "$BASE_DIR"/"$VERSION"/package/"$VERSION_CODENAME"/
}

memcheck() {
	# With ENABLE_DEBUG="OFF" testkdb_allplugins detects a memleak on buster, bullseye, bionic and opensuse 15.3,
	# therefore the tests are disabled for theses distribitions.
	# discussed in: https://github.com/ElektraInitiative/libelektra/pull/3530
	# see also: https://github.com/ElektraInitiative/libelektra/pull/3855
	if [ "$VERSION_CODENAME" = "buster" ] || [ "$VERSION_CODENAME" = "bullseye" ] || [ "$VERSION_CODENAME" = "bionic" ] || [ "$VERSION_CODENAME" = "opensuse-leap15.3" ]; then
		cmemcheck 'testkdb_allplugins'
	else
		cmemcheck
	fi
}

cmemcheck() {
	ctest -j "$CTEST_PARALLEL_LEVEL" --force-new-ctest-process \
		--output-on-failure --no-compress-output \
		-T MemCheck -LE memleak -E "$1"
}

# get version codename
VERSION_CODENAME=""
find_version_codename
install_elektra
run_updates
git_tag
update_fedora_changelog
update_debian_changelog
export_git_log
run_checks
create_source_package
build_package
cd "$BASE_DIR"
"$SCRIPTS_DIR"/release/sign-packages.sh "$BASE_DIR"/"$VERSION"/package/"$VERSION_CODENAME"
tar -czvf release.tar.gz ./"$VERSION"
