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

			if (jna) # necessary binding was built
				set (libelektra_jar ${CMAKE_BINARY_DIR}/src/bindings/jna/libelektra.jar)

				configure_file (
					"${CMAKE_CURRENT_SOURCE_DIR}/testmod_jni.h.in"
					"${CMAKE_CURRENT_BINARY_DIR}/testmod_jni.h"
					)

				include_directories (${CMAKE_CURRENT_BINARY_DIR})
				add_plugintest(jni MEMLEAK)
			endif ()
		else ()
			remove_plugin (jni "jni.h does not define JNI_VERSION_1_8")
		endif ()
		unset (CMAKE_REQUIRED_INCLUDES)
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
