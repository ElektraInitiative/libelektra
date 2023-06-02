include (LibAddMacros)
include (LibAddBinding)
include (SafeCheckSymbolExists)

if (DEPENDENCY_PHASE)
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

	if (JNI_FOUND)
		# init CMAKE_THREAD_LIBS_INIT
		find_package (Threads)

		set (DIRS ${JNI_INCLUDE_DIRS} ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
		set (LIBS ${CMAKE_THREAD_LIBS_INIT} ${JAVA_MAWT_LIBRARY} ${JNI_LIBRARIES} ${JAVA_AWT_LIBRARY} ${JAVA_JVM_LIBRARY}
		)# for check_symbol_exists
		set (CMAKE_REQUIRED_INCLUDES ${DIRS})
		safe_check_symbol_exists (JNI_VERSION_10 jni.h JNI_CORRECT_VERSION)

		if (NOT JNI_CORRECT_VERSION)
			remove_plugin (jni "jni.h (in ${CMAKE_REQUIRED_INCLUDES}) does not define JNI_VERSION_10")
		endif ()
		unset (CMAKE_REQUIRED_INCLUDES)
	else ()
		remove_plugin (jni "JNI not found, please review your JDK installation and consider setting JAVA_HOME accordingly")
	endif ()
endif ()

add_plugin (
	jni
	SOURCES jni.c
	INCLUDE_DIRECTORIES ${DIRS}
	LINK_LIBRARIES ${LIBS} COMPONENT libelektra${SO_VERSION}-java)

if (ADDTESTING_PHASE)
	check_binding_was_added ("jna" BINDING_WAS_ADDED)

	if (BUILD_TESTING
	    AND BINDING_WAS_ADDED
	    AND NOT ENABLE_ASAN)
		# do not install test, because it depends on build folder JNA fat jar
		add_plugintest (jni MEMLEAK USE_LINK_RPATH NO_INSTALL)
		target_include_directories (testmod_jni PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")

		# Generate header file
		set (libelektra_jar_all ${CMAKE_BINARY_DIR}/src/bindings/jna/libelektra/build/libs/libelektra-${KDB_VERSION}-all.jar)
		configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/testmod_jni.h.in" "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h")

		add_custom_target (jni_header DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h")
	else ()
		message (WARNING "JNA bindings are required for testing but binding was not enabled. JNI test deactivated.")
	endif ()
endif ()
