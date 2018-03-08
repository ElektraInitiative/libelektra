include (LibAddMacros)

find_package (PkgConfig)
pkg_check_modules (ZMQ QUIET libzmq)

if (ZMQ_FOUND)

	set (ADAPTER_NAME zeromq)

	## Build library
	set (ADAPTER_SRC_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/zeromq.c"
	)

	set (SOURCES ${ADAPTER_SRC_FILES})

	set (ADAPTER_LIBRARY elektra-io-adapter-${ADAPTER_NAME})

	add_lib (io-adapter-${ADAPTER_NAME}
		SOURCES ${SOURCES}
		LINK_ELEKTRA
			elektra-io
		LINK_LIBRARIES
			${ZMQ_LIBRARIES}
	)

	if (BUILD_FULL OR BUILD_STATIC)
		add_includes ("elektra-full" ${ZMQ_INCLUDE_DIR})
	endif ()
	if (BUILD_SHARED)
		target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${ZMQ_INCLUDE_DIR})
	endif ()

endif ()
