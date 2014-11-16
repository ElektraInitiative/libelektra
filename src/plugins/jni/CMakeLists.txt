include (LibAddMacros)

find_package (JNI)

if (JNI_FOUND)
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
	remove_plugin(jni "jni not found")
endif()
