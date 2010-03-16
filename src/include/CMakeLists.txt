#copy_file (kdbbackend)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/kdb.h.in"
	"${CMAKE_CURRENT_BINARY_DIR}/kdb.h"
	)

#copy_file (kdbloader)
#copy_file (kdbos)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/kdbprivate.h.in"
	"${CMAKE_CURRENT_BINARY_DIR}/kdbprivate.h"
	)

#copy_file (kdbtools)

configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/config.h.in"
	"${CMAKE_CURRENT_BINARY_DIR}/config.h"
	)

install_files (/include .h kdbbackend kdb kdbloader kdbos kdbprivate kdbtools config)
