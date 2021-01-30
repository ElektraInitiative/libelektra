set (SOURCE_FILE "${SOURCE_PATH}/testmod_jni.h.in")
set (DESTINATION_FILE "${DESTINATION_PATH}/testmod_jni.h")
set (CLASSPATH_FILE "${LIBELEKTRA5J_PATH}/classpath.txt")

set (libelektra_jar ${LIBELEKTRA5J_PATH}/target/libelektra5j-${KDB_VERSION}.jar)
file (READ "${LIBELEKTRA5J_PATH}/classpath.txt" jna)

configure_file ("${SOURCE_FILE}" "${DESTINATION_FILE}")
