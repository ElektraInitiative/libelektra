include (LibAddMacros)

find_package (JNI)

if (JNI_FOUND)
	find_library(JAVA_MAWT_LIBRARY
		mawt
		HINTS
		/usr/lib/jvm/java-6-openjdk-amd64/jre/lib/amd64/headless
		/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/headless
		/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/headless
		)

	IF(EXISTS ${JAVA_LIB_DIR}/xawt)
		LINK_DIRECTORIES(${JAVA_LIB_DIR}/xawt)
	ENDIF(EXISTS ${JAVA_LIB_DIR}/xawt)
	IF(EXISTS ${JAVA_LIB_DIR}/client)
		LINK_DIRECTORIES(${JAVA_LIB_DIR}/client)
	ENDIF(EXISTS ${JAVA_LIB_DIR}/client)

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
