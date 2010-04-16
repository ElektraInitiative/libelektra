configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/kdb.h"
	"${CMAKE_CURRENT_BINARY_DIR}/kdb.h"
	)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/kdbprivate.h"
	"${CMAKE_CURRENT_BINARY_DIR}/kdbprivate.h"
	)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/config.h"
	"${CMAKE_CURRENT_BINARY_DIR}/config.h"
	)

install_files (/include .h kdbbackend kdb kdbloader kdbos kdbprivate kdbtools config)
