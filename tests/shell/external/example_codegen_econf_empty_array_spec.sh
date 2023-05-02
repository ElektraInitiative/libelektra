#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL CODEGEN ECONF EMPTY ARRAY SPEC
echo

if command -v realpath; then
	if command -v pkg-config; then
		if ! pkg-config elektra; then
			echo "Elektra not installed, will skip"
			exit 0
		fi
	else
		echo "pkg-config not installed, will skip"
		exit 0
	fi
else
	echo "realpath is not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/codegen/econf"

check_if_parent_exists() {
	if grep -e "spec:\/sw\/example\/econf/\#0\/current\/format\/#$"; then
		echo "array parent was found"
		return 0
	else
	  echo "did not find array parent spec:/sw/example/econf/#0/current/format/#"
		return 1
	fi
}

do_tests() {
	KEY="/sw/example/econf/#0/current"
	UKEY="user:$KEY"
	SPECKEY="spec:$KEY"

	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$KEY"
	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"

	"$KDB" mount `pwd`/spec.ini "$SPECKEY" ni
	"$KDB" import "$SPECKEY" ni < "$EXTERNAL_FOLDER/spec.ini"
	"$KDB" spec-mount "$KEY"

	"$KDB" ls "spec:/sw/example/econf/#0" | check_if_parent_exists

	"$KDB" meta-set "$KEY/format" array "#2"
	"$KDB" ls "spec:/sw/example/econf/#0" | check_if_parent_exists
}

echo "Testing build with cmake"

cd "$EXTERNAL_FOLDER" || exit
mkdir build
cd build || exit

# manually set Elektra_DIR and KDB to support non-standard install locations
cmake ../cmake -DElektra_DIR:PATH="$(realpath $(dirname "$0")/../../cmake/Elektra)" -DKDB:PATH="$KDB"
succeed_if "could not run cmake"

cmake --build .
succeed_if "could not build cmake project"

do_tests

cd ..
rm -r build

echo "Testing build with pkgconfig"

cd "$EXTERNAL_FOLDER/pkgconfig" || exit
# set KDB to support non-standard install locations
KDB="$KDB" make
succeed_if "could not build pkgconfig project"

do_tests

make clean

end_script gen