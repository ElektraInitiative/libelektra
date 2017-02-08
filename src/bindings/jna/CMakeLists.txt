find_package(Java 1.8 REQUIRED)
find_program(MAVEN_EXECUTABLE mvn)

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

	# if the dependency is already installed in the local repository, then there is no need to run maven again
	execute_process (
		COMMAND ${MAVEN_EXECUTABLE} dependency:get -o -q -B -Dartifact=org.libelektra:libelektra4j:${KDB_VERSION}
		RESULT_VARIABLE MAVEN_ARTIFACT_INSTALLED
		OUTPUT_QUIET
		ERROR_QUIET
	)
	if (NOT "${MAVEN_ARTIFACT_INSTALLED}" STREQUAL "0")

		# compile it already so build errors get detected before installing
		add_custom_target (
			jna ALL
			COMMAND ${MAVEN_EXECUTABLE} -q -B -DskipTests=true compile
			WORKING_DIRECTORY libelektra4j
		)

		# prepare the installation script
		configure_file(
			"${CMAKE_CURRENT_SOURCE_DIR}/maven_install.cmake.in"
			"${CMAKE_CURRENT_BINARY_DIR}/maven_install.cmake"
		)
		install (SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/maven_install.cmake)

	else () # if already installed
		message ("JNA bindings already installed in local maven repository, not building them again")
	endif () # if already installed
	# TODO handle testing

else ()
	remove_binding (jna, "Maven Executable not found, which is required to build the jna bindings")
endif ()
