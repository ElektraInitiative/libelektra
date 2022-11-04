find_program (GRADLE_EXECUTABLE gradle)
find_package (Java QUIET)

set (MIN_JAVA_VERSION "11")
set (MIN_GRADLE_VERSION "6.0")

macro (java_exclude_test test)
	string (APPEND GRADLE_TEST_EXCLUDES "\t\texcludeTestsMatching '*.${test}'\n")
endmacro ()

macro (java_test_needs_plugin test plugin)
	list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
	if (plugin_index GREATER -1)
		java_exclude_test (${test})
	endif (plugin_index GREATER -1)
endmacro (
	java_test_needs_plugin
	test
	plugin)

if (Java_JAVAC_EXECUTABLE)

	if (NOT Java_VERSION)
		exclude_binding (jna "unable to determine version of javac (java compiler)")
		return ()
	endif ()

	# ~~~
	# the version schemes are different on the jdks
	# on osx/oracle jdk its called java 9.0.1
	# osx/oracle jdk 8 is called java 1.8.x
	# on ubuntu/open jdk its called java 1.9.0
	# therefore this workaround should work in all cases
	# as 9.x.x > 1.8 and 1.8.x > 1.8 and 1.9.x > 1.8
	# ~~~
	if ((${Java_VERSION} VERSION_GREATER ${MIN_JAVA_VERSION}) OR (${Java_VERSION} VERSION_EQUAL ${MIN_JAVA_VERSION}))
		if (NOT BUILD_SHARED)
			# See also: https://travis-ci.org/sanssecours/elektra/jobs/445840045
			exclude_binding (jna "it can only be built if `BUILD_SHARED` is enabled")
			return ()
		endif ()

		if (GRADLE_EXECUTABLE) # set by find_program
			# check Gradle version
			execute_process (
				COMMAND bash "-c" "${GRADLE_EXECUTABLE} -v | grep 'Gradle *' | egrep -o '[0-9]+\.[0-9]+(\.[0-9]+)?'"
				OUTPUT_VARIABLE GRADLE_VERSION
				OUTPUT_STRIP_TRAILING_WHITESPACE)

			if ((${GRADLE_VERSION} VERSION_GREATER ${MIN_GRADLE_VERSION}) OR (${GRADLE_VERSION} VERSION_EQUAL
											  ${MIN_GRADLE_VERSION}))
				add_binding (jna)

				file (MAKE_DIRECTORY libelektra)

				# generate test exclusions based on specific plugins and environment
				set (GRADLE_TEST_EXCLUDES "")
				java_test_needs_plugin (GOptsTest gopts)

				# first fill the elektra version in the pom file @ONLY to avoid replacing pom.xml placeholders which also
				# use the format ${}
				configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/version-settings.gradle.in"
						"${CMAKE_CURRENT_BINARY_DIR}/version-settings.gradle" @ONLY)
				configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/libelektra/test-exclusion.gradle.in"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/test-exclusion.gradle" @ONLY)

				# the build system calls Gradle to recompile the binding, if we change any of the following source files.
				set (JNA_BINDING_PREFIX libelektra)
				set (JNA_BINDING_NAMESPACE java/org/libelektra)
				set (JNA_BINDING_SOURCE_DIRECTORY ${JNA_BINDING_PREFIX}/src/main/${JNA_BINDING_NAMESPACE})
				set (JNA_BINDING_SOURCE_DIRECTORY_PLUGIN ${JNA_BINDING_SOURCE_DIRECTORY}/plugin)
				set (JNA_BINDING_SOURCE_DIRECTORY_EXCEPTION ${JNA_BINDING_SOURCE_DIRECTORY}/exception)
				set (JNA_BINDING_SOURCE_DIRECTORY_TEST ${JNA_BINDING_PREFIX}/src/test/${JNA_BINDING_NAMESPACE})
				set (JNA_BINDING_SOURCE_DIRECTORY_TEST_PLUGIN ${JNA_BINDING_SOURCE_DIRECTORY_TEST}/plugin)

				# copy the source files at configure time, so we can ask Gradle to list source files and plugin JARs
				file (COPY "${CMAKE_CURRENT_SOURCE_DIR}" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/..")

				execute_process (
					COMMAND "${CMAKE_CURRENT_BINARY_DIR}/gradlew" "-q" "printSourceFiles"
					OUTPUT_VARIABLE JNA_BINDING_SOURCE_FILES
					WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
				string (REPLACE "\n" ";" JNA_BINDING_SOURCE_FILES "${JNA_BINDING_SOURCE_FILES}")

				execute_process (
					COMMAND "${CMAKE_CURRENT_BINARY_DIR}/gradlew" "-q" ":plugins:printJarPaths"
					OUTPUT_VARIABLE PLUGIN_JAR_PATHS
					WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
				string (REPLACE "-SNAPSHOT.jar" "-${KDB_VERSION}.jar" PLUGIN_JAR_PATHS "${PLUGIN_JAR_PATHS}")
				string (REPLACE "\n" ";" PLUGIN_JAR_PATHS "${PLUGIN_JAR_PATHS}")

				set (
					EXTRA_JNA_BINDING_SOURCE_FILES
					${JNA_BINDING_PREFIX}/build.gradle
					${JNA_BINDING_PREFIX}/dependencies.gradle
					plugins/build.gradle
					plugins/dependencies.gradle
					process/build.gradle
					process/dependencies.gradle
					build.gradle
					gradle.properties
					java-library.gradle
					java-shared.gradle
					maven-publish.gradle
					settings.gradle)

				# compile the source files and build the jar. We copy the source files into the build folder as that is the
				# easiest way to handle the integration between CMake and Gradle.
				add_custom_command (
					OUTPUT
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}-javadoc.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}-sources.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/publications/libelektra/libelektra-${KDB_VERSION}.pom.xml"
						"${CMAKE_CURRENT_BINARY_DIR}/process/build/libs/process-${KDB_VERSION}.jar"
						${PLUGIN_JAR_PATHS}
					COMMAND ${CMAKE_COMMAND} ARGS -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}"
						"${CMAKE_CURRENT_BINARY_DIR}"
					COMMAND ${GRADLE_EXECUTABLE} --stacktrace -q --console=plain clean assemble
					WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
					DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/libelektra/test-exclusion.gradle"
						"${CMAKE_CURRENT_BINARY_DIR}/version-settings.gradle" ${JNA_BINDING_SOURCE_FILES}
						${EXTRA_JNA_BINDING_SOURCE_FILES})

				create_lib_symlink (libelektra-${KDB_VERSION}.jar libelektra.jar java-elektra JAVA)
				create_lib_symlink (libelektra-${KDB_VERSION}-javadoc.jar libelektra-javadoc.jar java-elektra JAVA)
				create_lib_symlink (libelektra-${KDB_VERSION}-sources.jar libelektra-sources.jar java-elektra JAVA)
				create_lib_symlink (libelektra-${KDB_VERSION}.pom.xml libelektra.pom.xml java-elektra JAVA)

				add_custom_target (
					jna ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}.jar")

				install (
					FILES
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}-all.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}-javadoc.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/libs/libelektra-${KDB_VERSION}-sources.jar"
						"${CMAKE_CURRENT_BINARY_DIR}/libelektra/build/publications/libelektra/libelektra-${KDB_VERSION}.pom.xml"
						"${CMAKE_CURRENT_BINARY_DIR}/process/build/libs/process-${KDB_VERSION}.jar"
					DESTINATION "share/java"
					COMPONENT java-elektra)

				foreach (PLUGIN_JAR ${PLUGIN_JAR_PATHS})
					message (STATUS "Adding Java plugin: " "${PLUGIN_JAR}")
					install (
						FILES "${CMAKE_CURRENT_BINARY_DIR}/${PLUGIN_JAR}"
						DESTINATION "share/java/plugins"
						COMPONENT java-elektra)
				endforeach (PLUGIN_JAR ${PLUGIN_JAR_PATHS})

				# we cannot set DYLD_LIBRARY_PATH on new macOS versions, making the kdb tests fail if its not installed in
				# the system
				if (BUILD_TESTING
				    AND NOT ENABLE_ASAN
				    AND NOT
					((CMAKE_SYSTEM_NAME STREQUAL "Darwin")
					 AND NOT (CMAKE_SYSTEM_VERSION VERSION_LESS 15)
					 OR (NOT BUILD_TESTING)))
					add_test (
						NAME testjna_gradle
						COMMAND ${GRADLE_EXECUTABLE} --stacktrace -q --console=plain test
						WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/libelektra")

					set_property (TEST testjna_gradle PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")
					set_property (TEST testjna_gradle PROPERTY LABELS kdbtests bindings memleak)
				endif ()
			else ()
				exclude_binding (
					jna
					"Gradle version ${GRADLE_VERSION} found, but at least version ${MIN_GRADLE_VERSION} is required to build the jna bindings"
				)
			endif ()
		else ()
			exclude_binding (jna "Gradle not found, but is required to build the jna bindings")
		endif ()
	else ()
		exclude_binding (jna "At least Java ${MIN_JAVA_VERSION} is required, but ${Java_VERSION_STRING} was found")
	endif ()
else ()
	exclude_binding (jna "javac (java compiler) not found, which is only included in a jdk")
endif ()

mark_as_advanced (GRADLE_EXECUTABLE)
