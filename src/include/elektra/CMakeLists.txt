install (FILES error.h
	       "${CMAKE_CURRENT_BINARY_DIR}/errors.h"
	       "${CMAKE_CURRENT_BINARY_DIR}/errorcodes.h"
	       "${CMAKE_CURRENT_BINARY_DIR}/errorsprivate.h"
	       conversion.h
	       types.h
	 DESTINATION include/${TARGET_INCLUDE_FOLDER}/elektra)
