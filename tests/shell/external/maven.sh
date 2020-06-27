@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL JAVA EXAMPLE
echo

if ! command -v mvn; then
	echo "Maven not installed, will skip"
	exit 0
fi

if ! test -f /usr/share/java/libelektra4j.jar; then
	echo "libelektra4j.jar not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/external/java/read-keys-example"

echo "Testing build with mvn"

cd "$EXTERNAL_FOLDER"

mvn org.apache.maven.plugins:maven-install-plugin:install-file \
	-Dfile=/usr/share/java/libelektra4j.jar \
	-DpomFile=/usr/share/java/libelektra4j.pom.xml

mvn clean package test

rm -r target

end_script java
