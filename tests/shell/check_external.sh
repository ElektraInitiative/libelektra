@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL
echo

if pkg-config elektra
then
	echo "Installed Elektra will be used"
	echo "We are assuming it is configured similarly"
	echo "The test will fail if installed version does not use"
	echo "same KDB."
else
	echo "Elektra or pkg-config not installed, will exit"
	exit
fi


check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/external"

set -x

do_tests()
{
	SKEY=system/test/myapp/key
	UKEY=user/test/myapp/key

	"$KDB" rm "$SKEY"
	"$KDB" rm "$UKEY"

	VALUE="Hello World"
	"$KDB" set "$SKEY"  "$VALUE"
	succeed_if "could not set key $SKEY"

	./application
	./application | grep "$VALUE"
	succeed_if "application did not output $VALUE"

	VALUE="More world"
	"$KDB" set "$UKEY"  "$VALUE"
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

cmake ../cmake
succeed_if "could not run cmake"

make
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
