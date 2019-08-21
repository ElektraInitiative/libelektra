configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdb.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdb.h")

install (
	FILES error.h conversion.h types.h "${CMAKE_CURRENT_BINARY_DIR}/kdb.h"
	DESTINATION include/${TARGET_INCLUDE_FOLDER}/elektra
	COMPONENT libelektra-dev)

