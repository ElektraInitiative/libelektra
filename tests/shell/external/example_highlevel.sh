#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL HIGHLEVEL
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

if ! kdb plugin-list | grep -xq ni; then
	echo "ni plugin not found, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/highlevel"

do_tests() {
	KEY=/sw/example/highlevel/#0/current
	UKEY="user:$KEY"
	SPECKEY="spec:$KEY"

	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$KEY"
	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"

	"$KDB" mount "highlevel_spec.ini" "$SPECKEY" ni
	"$KDB" import "$SPECKEY" ni < "$EXTERNAL_FOLDER/spec.ini"
	"$KDB" spec-mount "$KEY"

	./application
	succeed_if "application could not read default config (spec)"

	MYSTRING="Hello World"
	MYINT="29"
	MYDOUBLE="4.4242"
	MYFLOATSIZE="5"
	MYFLOAT0="3.14"
	MYFLOAT1="15"
	MYFLOAT2="9265.359"
	MYFLOAT3="2.718282"
	MYFLOAT4="1.4142"
	"$KDB" set "$UKEY/mystring" "$MYSTRING"
	"$KDB" set "$UKEY/myint" "$MYINT"
	"$KDB" set "$UKEY/mydouble" "$MYDOUBLE"
	"$KDB" set "$UKEY/myfloatarray/#0" "$MYFLOAT0"
	"$KDB" set "$UKEY/myfloatarray/#1" "$MYFLOAT1"
	"$KDB" set "$UKEY/myfloatarray/#2" "$MYFLOAT2"
	"$KDB" set "$UKEY/myfloatarray/#3" "$MYFLOAT3"
	"$KDB" set "$UKEY/myfloatarray/#4" "$MYFLOAT4"
	"$KDB" meta-set "$UKEY/myfloatarray" array "#4"

	./application
	succeed_if "application could not read changed config"

	"$KDB" set "$UKEY/print" "1"

	./application
	succeed_if "application could not read changed config"
	./application | grep "mystring: $MYSTRING"
	succeed_if "application did not print mystring"
	./application | grep "myint: $MYINT"
	succeed_if "application did not print myint"
	./application | grep "mydouble: $MYDOUBLE"
	succeed_if "application did not print mydouble"
	./application | grep "sizeof(myfloatarray): $MYFLOATSIZE"
	succeed_if "application did not print size of myfloatarray"
	./application | grep "myfloatarray\\[0\\]: $MYFLOAT0"
	succeed_if "application did not print myfloatarray[0]"
	./application | grep "myfloatarray\\[1\\]: $MYFLOAT1"
	succeed_if "application did not print myfloatarray[1]"
	./application | grep "myfloatarray\\[2\\]: $MYFLOAT2"
	succeed_if "application did not print myfloatarray[2]"
	./application | grep "myfloatarray\\[3\\]: $MYFLOAT3"
	succeed_if "application did not print myfloatarray[3]"
	./application | grep "myfloatarray\\[4\\]: $MYFLOAT4"
	succeed_if "application did not print myfloatarray[4]"

	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"
	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$KEY"
}

echo "Testing build with cmake"

cd "$EXTERNAL_FOLDER"
mkdir build
cd build

# manually set Elektra_DIR and KDB to support non-standard install locations
cmake ../cmake -DElektra_DIR:PATH="$(realpath $(dirname $0)/../../cmake/Elektra)"
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

end_script gen
