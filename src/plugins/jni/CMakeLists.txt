include (LibAddMacros)
include (LibAddBinding)
include (SafeCheckSymbolExists)

if (DEPENDENCY_PHASE)
	if (APPLE)
		execute_process (
			COMMAND "/usr/libexec/java_home --failfast"
			ERROR_QUIET
			OUTPUT_VARIABLE JAVA_HOME
			OUTPUT_STRIP_TRAILING_WHITESPACE)
	endif ()

	find_package (JNI QUIET)

	if (JNI_FOUND)
		set (DIRS ${JNI_INCLUDE_DIRS} ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
		set (LIBS ${JAVA_MAWT_LIBRARY} ${JNI_LIBRARIES} ${JAVA_AWT_LIBRARY} ${JAVA_JVM_LIBRARY}) # for check_symbol_exists
		set (CMAKE_REQUIRED_INCLUDES ${DIRS})
		safe_check_symbol_exists (JNI_VERSION_1_8 jni.h JNI_CORRECT_VERSION)
		unset (CMAKE_REQUIRED_INCLUDES)

		if (NOT JNI_CORRECT_VERSION)
			remove_plugin (jni "jni.h does not define JNI_VERSION_1_8")
		endif ()
	else ()
		remove_plugin (jni "jni not found")
	endif ()
endif ()

add_plugin (
	jni
	SOURCES jni.c
	INCLUDE_DIRECTORIES ${DIRS}
	LINK_LIBRARIES ${LIBS} COMPONENT libelektra4-java
	ONLY_SHARED)

if (ADDTESTING_PHASE)
	check_binding_was_added ("jna" BINDING_WAS_ADDED)
	find_program (MAVEN_EXECUTABLE mvn)

	if (ENABLE_BROKEN_TESTS)
		if (BUILD_TESTING
		    AND BINDING_WAS_ADDED
		    AND MAVEN_EXECUTABLE)
			add_plugintest (jni MEMLEAK)
			include_directories (${CMAKE_CURRENT_BINARY_DIR})

			# Generate header file
			set (LIBELEKTRA4J_PATH ${CMAKE_BINARY_DIR}/src/bindings/jna/libelektra4j)
			add_custom_command (
				COMMAND ${MAVEN_EXECUTABLE} ARGS dependency:build-classpath -q -Dmdep.outputFile=classpath.txt
				COMMAND
					${CMAKE_COMMAND} ARGS -D SOURCE_PATH=${CMAKE_CURRENT_SOURCE_DIR} -D
					DESTINATION_PATH=${CMAKE_CURRENT_BINARY_DIR} -D LIBELEKTRA4J_PATH=${LIBELEKTRA4J_PATH} -D
					KDB_VERSION=${KDB_VERSION} -P ${CMAKE_SOURCE_DIR}/scripts/cmake/ElektraClasspath.cmake
				OUTPUT "${LIBELEKTRA4J_PATH}/classpath.txt" "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h"
				WORKING_DIRECTORY ${LIBELEKTRA4J_PATH}
				DEPENDS "${LIBELEKTRA4J_PATH}/pom.xml jna")
			add_custom_target (jni_header DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h")
		else ()
			message (WARNING "jna bindings are required for testing, test deactivated")
		endif ()
	endif (ENABLE_BROKEN_TESTS)
endif ()
