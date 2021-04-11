@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL JAVA MAVEN EXAMPLE
echo

if ! command -v mvn; then
	echo "Maven not installed, will skip"
	exit 0
fi

if ! test -f @CMAKE_INSTALL_PREFIX@/share/java/libelektra.jar; then
	echo "libelektra.jar not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/external/java/read-keys-example"

echo "Testing build with mvn"

cd "$EXTERNAL_FOLDER"

mvn org.apache.maven.plugins:maven-install-plugin:install-file \
	-Dfile=@CMAKE_INSTALL_PREFIX@/share/java/libelektra.jar \
	-DpomFile=@CMAKE_INSTALL_PREFIX@/share/java/libelektra.pom.xml

mvn clean package test

rm -r target

end_script maven
