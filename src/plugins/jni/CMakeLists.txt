include (LibAddMacros)

find_package (JNI)

if (JNI_FOUND)
	check_symbol_exists(JNI_VERSION_1_8 jni.h JNI_CORRECT_VERSION)
	if (JNI_CORRECT_VERSION)
		add_plugin(jni
			SOURCES
				jni.c
			INCLUDE_DIRECTORIES
				${JNI_INCLUDE_DIRS}
				${JAVA_INCLUDE_PATH}
				${JAVA_INCLUDE_PATH2}
				${JAVA_AWT_INCLUDE_PATH}
			LINK_LIBRARIES
				${JAVA_MAWT_LIBRARY}
				${JNI_LIBRARIES}
				${JAVA_AWT_LIBRARY}
				${JAVA_JVM_LIBRARY}
			)
	else()
		remove_plugin(jni "jni.h does not define JNI_VERSION_1_8")
	endif()
else()
	remove_plugin(jni "jni not found")
endif()
