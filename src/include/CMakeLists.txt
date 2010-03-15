configure_file (
	"${PROJECT_SOURCE_DIR}/src/include/config.h.in"
	"${PROJECT_BINARY_DIR}/src/include/config.h"
	)

configure_file (
	"${PROJECT_SOURCE_DIR}/src/include/kdb.h.in"
	"${PROJECT_BINARY_DIR}/src/include/kdb.h"
	)

configure_file (
	"${PROJECT_SOURCE_DIR}/src/include/kdbprivate.h.in"
	"${PROJECT_BINARY_DIR}/src/include/kdbprivate.h"
	)

#TODO: does nothing?
include_directories("${PROJECT_BINARY_DIR}/src/include")
