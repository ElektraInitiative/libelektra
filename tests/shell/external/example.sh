#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL EXAMPLE
echo

if command -v pkg-config; then
	if ! pkg-config elektra; then
		echo "Elektra not installed, will skip"
		exit 0
	fi
else
	echo "pkg-config not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/external"

do_tests() {
	SKEY=system:/test/myapp/key
	UKEY=user:/test/myapp/key

	"$KDB" rm "$SKEY"
	"$KDB" rm "$UKEY"

	VALUE="Hello World"
	"$KDB" set "$SKEY" "$VALUE"
	succeed_if "could not set key $SKEY"

	./application
	./application | grep "$VALUE"
	succeed_if "application did not output $VALUE"

	VALUE="More world"
	"$KDB" set "$UKEY" "$VALUE"
	succeed_if "could not set key $SKEY"

	./application
	./application | grep "$VALUE"
	succeed_if "application did not prefer $UKEY with $VALUE"

	"$KDB" rm "$SKEY"
	"$KDB" rm "$UKEY"
}

echo "Testing build with cmake"

cd "$EXTERNAL_FOLDER"
mkdir build
cd build

# manually set Elektra_DIR and KDB to support non-standard install locations
cmake ../cmake -DElektra_DIR:PATH="$(realpath $(dirname $0)/../../scripts/cmake/Elektra)"
succeed_if "could not run cmake"

cmake --build .
succeed_if "could not build cmake project"

do_tests
do_tests

cd ..
rm -r build

echo "Testing build with pkgconfig"

cd "$EXTERNAL_FOLDER/pkgconfig"
make
succeed_if "could not build pkgconfig project"

do_tests
do_tests

rm application

end_script example
