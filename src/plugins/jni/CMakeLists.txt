include (LibAddMacros)
include (LibAddBinding)
include (SafeCheckSymbolExists)

if (DEPENDENCY_PHASE)
	set (JAVA_HOME $ENV{JAVA_HOME})
	if (APPLE)
		if (NOT DEFINED JAVA_HOME)
			execute_process (
				COMMAND "/usr/libexec/java_home" "--failfast"
				ERROR_QUIET
				OUTPUT_VARIABLE JAVA_HOME
				OUTPUT_STRIP_TRAILING_WHITESPACE)
		endif ()
	endif ()

	find_package (JNI)

	message (INFO "JNI_INCLUDE_DIRS: ${JNI_INCLUDE_DIRS}")
	message (INFO "JNI_LIBRARIES: ${JNI_LIBRARIES}")
	message (INFO "JNI_FOUND: ${JNI_FOUND}")
	message (INFO "JAVA_AWT_LIBRARY: ${JAVA_AWT_LIBRARY}")
	message (INFO "JAVA_JVM_LIBRARY: ${JAVA_JVM_LIBRARY}")
	message (INFO "JNI_INCLUDE_DIRS: ${JNI_INCLUDE_DIRS}")
	message (INFO "JAVA_INCLUDE_PATH: ${JAVA_INCLUDE_PATH}")
	message (INFO "JAVA_INCLUDE_PATH2: ${JAVA_INCLUDE_PATH2}")
	message (INFO "JAVA_AWT_INCLUDE_PATH: ${JAVA_AWT_INCLUDE_PATH}")

	if (JNI_FOUND)
		set (DIRS ${JNI_INCLUDE_DIRS} ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
		set (LIBS ${JAVA_MAWT_LIBRARY} ${JNI_LIBRARIES} ${JAVA_AWT_LIBRARY} ${JAVA_JVM_LIBRARY}) # for check_symbol_exists
		set (CMAKE_REQUIRED_INCLUDES ${DIRS})
		safe_check_symbol_exists (JNI_VERSION_9 jni.h JNI_CORRECT_VERSION)
		unset (CMAKE_REQUIRED_INCLUDES)

		if (NOT JNI_CORRECT_VERSION)
			remove_plugin (jni "jni.h does not define JNI_VERSION_9")
		endif ()
	else ()
		remove_plugin (jni "jni not found")
	endif ()
endif ()

add_plugin (
	jni
	SOURCES jni.c
	INCLUDE_DIRECTORIES ${DIRS}
	LINK_LIBRARIES ${LIBS} COMPONENT libelektra${SO_VERSION}-java
	ONLY_SHARED USE_LINK_RPATH)

if (ADDTESTING_PHASE)
	check_binding_was_added ("jna" BINDING_WAS_ADDED)

	if (BUILD_TESTING AND BINDING_WAS_ADDED)
		add_plugintest (jni MEMLEAK)
		include_directories (${CMAKE_CURRENT_BINARY_DIR})

		# Generate header file
		set (libelektra_jar_all ${CMAKE_BINARY_DIR}/src/bindings/jna/libelektra/build/libs/libelektra-${KDB_VERSION}-all.jar)
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/testmod_jni.h.in" "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h")

		add_custom_target (jni_header DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h")
	else ()
		message (WARNING "JNA bindings are required for testing but binding was not enabled. JNI test deactivated.")
	endif ()
endif ()
