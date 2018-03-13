include (LibAddMacros)

find_package (PkgConfig)
pkg_check_modules (ZMQ QUIET libzmq)

if (ZMQ_FOUND)

	set (ADAPTER_OBJECT io-adapter-zeromq)

	## Build library
	set (ADAPTER_SRC_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/zeromq.c"
	)

	set (SOURCES ${ADAPTER_SRC_FILES})

	add_library (${ADAPTER_OBJECT} OBJECT ${SOURCES})
	target_include_directories (${ADAPTER_OBJECT} SYSTEM PUBLIC ${ZMQ_INCLUDE_DIR})

	add_subdirectory (hub)

endif ()
