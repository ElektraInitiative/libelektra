find_package (Java 1.8 REQUIRED)
find_program (MAVEN_EXECUTABLE mvn)

if (NOT MAVEN_EXECUTABLE STREQUAL "MAVEN_EXECUTABLE-NOTFOUND")
	file (
		MAKE_DIRECTORY libelektra4j
	)

	# first fill the elektra version in the pom file
	# @ONLY to avoid replacing pom.xml placeholders which also use the format ${}
	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/libelektra4j/pom.xml.in"
		"${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/pom.xml"
		@ONLY
	)

	# then copy over the source files in the build folder as thats the easiest way to handle it
	file (
		COPY "${CMAKE_CURRENT_SOURCE_DIR}/libelektra4j"
		DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
	)

	# compile it and build the jar, but only test in test phase
	add_custom_target (
		jna ALL
		COMMAND ${MAVEN_EXECUTABLE} -q -B -DskipTests=true package
		WORKING_DIRECTORY libelektra4j
	)

	# install_jar from UseJava doesn't really do more than that
	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/target/libelektra4j-${KDB_VERSION}.jar"
		DESTINATION "share/java"
	)

	# and add maven test to be executed with ctest in the testing phase
	add_test (
		NAME testjna_maven
		COMMAND ${MAVEN_EXECUTABLE} -B test
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/libelektra4j"
	)
else ()
	remove_binding (jna "Maven Executable not found, which is required to build the jna bindings")
endif ()
