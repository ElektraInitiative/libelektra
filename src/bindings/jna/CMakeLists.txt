find_package (Java)
find_program (MAVEN_EXECUTABLE mvn)

if (Java_FOUND) # set by find_package
if (Java_VERSION_MAJOR GREATER 0 AND Java_VERSION_MINOR GREATER 7)
if (MAVEN_EXECUTABLE) # set by find_program
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

	# required to have a recognizable name for the pom upon installation in case other software installs
	# a pom to the target directory and calls it pom.xml
	file (
		GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/libelektra4j-${KDB_VERSION}.pom.xml"
		INPUT "${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/pom.xml"
	)

	# then copy over the source files in the build folder as thats the easiest way to handle it
	file (
		COPY "${CMAKE_CURRENT_SOURCE_DIR}/libelektra4j"
		DESTINATION "${CMAKE_CURRENT_BINARY_DIR}"
	)

	# compile it and build the jar
	add_custom_target (
		jna ALL
		COMMAND ${MAVEN_EXECUTABLE} -q -B -DskipTests=true package
		WORKING_DIRECTORY libelektra4j
	)

	install (
		FILES
			"${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/target/libelektra4j-${KDB_VERSION}.jar"
			"${CMAKE_CURRENT_BINARY_DIR}/libelektra4j/libelektra4j-${KDB_VERSION}.pom.xml"
		DESTINATION "share/java"
	)

	# and add maven test to execute with ctest in the testing phase
	add_test (
		NAME testjna_maven
		COMMAND ${MAVEN_EXECUTABLE} -B test
		WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/libelektra4j"
	)
else (MAVEN_EXECUTABLE)
	remove_binding (jna "Maven Executable not found, but required to build the jna bindings")
endif (MAVEN_EXECUTABLE)
else (Java_VERSION_MAJOR GREATER 0 AND Java_VERSION_MINOR GREATER 7)
	remove_binding (jna "Java is not greater version 8, but ${Java_VERSION} was found")
endif (Java_VERSION_MAJOR GREATER 0 AND Java_VERSION_MINOR GREATER 7)
else (Java_FOUND)
	remove_binding (jna "Java not found, but required to build the jna bindings")
endif (Java_FOUND)
