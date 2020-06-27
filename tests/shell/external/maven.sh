@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL JAVA EXAMPLE
echo

if command -v mvn; then
	if ! mvn; then
		echo "Maven not installed, will skip"
		exit 0
	fi
else
	echo "Maven not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/external/java/read-keys-example"

echo "Testing build with mvn"

cd "$EXTERNAL_FOLDER"
mvn clean package test

rm -r target

end_script java
