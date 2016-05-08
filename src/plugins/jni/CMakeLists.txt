include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (JNI)

	if (JNI_FOUND)
		set (CMAKE_REQUIRED_INCLUDES
					${JNI_INCLUDE_DIRS}
					${JAVA_INCLUDE_PATH}
					${JAVA_INCLUDE_PATH2}
					${JAVA_AWT_INCLUDE_PATH}
				)
		include (CheckSymbolExists)
		check_symbol_exists (JNI_VERSION_1_8 jni.h JNI_CORRECT_VERSION)
		if (JNI_CORRECT_VERSION)

			list (FIND BINDINGS "jna" FINDEX)
			if (BUILD_TESTING AND FINDEX GREATER -1)
				set (libelektra_jar ${CMAKE_BINARY_DIR}/src/bindings/jna/libelektra.jar)

				configure_file (
					"${CMAKE_CURRENT_SOURCE_DIR}/testmod_jni.h.in"
					"${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h"
					)

				include_directories (${CMAKE_CURRENT_BINARY_DIR})
				add_plugintest(jni MEMLEAK)
			else ()
				message (WARNING "jna bindings are required for testing, test deactivated")
			endif ()
		else ()
			remove_plugin (jni "jni.h does not define JNI_VERSION_1_8")
		endif ()
	else ()
		remove_plugin (jni "jni not found")
	endif ()
endif ()

add_plugin(jni
	SOURCES
		jni.c
	INCLUDE_DIRECTORIES
		${CMAKE_REQUIRED_INCLUDES}
	LINK_LIBRARIES
		${JAVA_MAWT_LIBRARY}
		${JNI_LIBRARIES}
		${JAVA_AWT_LIBRARY}
		${JAVA_JVM_LIBRARY}
	)

unset (CMAKE_REQUIRED_INCLUDES)
