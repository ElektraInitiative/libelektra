@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL
echo

echo "Requires installed Elektra"

check_version

EXTERNAL_FOLDER=@CMAKE_SOURCE_DIR@/external

do_tests()
{
	SKEY=system/test/myapp/key
	UKEY=user/test/myapp/key
	VALUE="Hello World"
	kdb set "$SKEY"  "$VALUE"
	succeed_if "could not set key $SKEY"

	./application
	./application | grep "$VALUE"
	succeed_if "application did not output $VALUE"

	VALUE="More world"
	kdb set "$UKEY"  "$VALUE"
	succeed_if "could not set key $SKEY"

	./application
	./application | grep "$VALUE"
	succeed_if "application did not prefer $UKEY with $VALUE"

	kdb rm "$SKEY"
	kdb rm "$UKEY"
}



echo "Testing build with cmake"

cd $EXTERNAL_FOLDER
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

cd $EXTERNAL_FOLDER/pkgconfig
make
succeed_if "could not build pkgconfig project"

do_tests
do_tests

rm application

end_script gen
