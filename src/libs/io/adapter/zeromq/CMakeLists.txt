include (LibAddMacros)

find_package (ZeroMQ QUIET)

if (ZeroMQ_FOUND)

	try_compile (
		CAN_LINK_ZMQ "${CMAKE_BINARY_DIR}"
		"${CMAKE_CURRENT_SOURCE_DIR}/try_compile_zeromq.c"
		CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ZeroMQ_INCLUDE_DIRS} -DLINK_LIBRARIES:PATH=${ZeroMQ_LIBRARIES})

	if (NOT CAN_LINK_ZMQ)
		message (
			WARNING
				"ZeroMQ I/O adapter disabled since linking against ZeroMQ failed. Targets that depend on this adapter will fail to compile."
		)
	else ()
		set (ADAPTER_OBJECT io-adapter-zeromq)

		# Build library
		set (ADAPTER_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/zeromq.c")

		set (SOURCES ${ADAPTER_SRC_FILES})

		add_library (${ADAPTER_OBJECT} OBJECT ${SOURCES})
		target_include_directories (${ADAPTER_OBJECT} SYSTEM PUBLIC ${ZeroMQ_INCLUDE_DIRS})

		# NOTE CMAKE_POSITION_INDEPENDENT_CODE does not seem to work (e.g. CMake 3.9.2 on Travis)
		target_compile_options (${ADAPTER_OBJECT} PRIVATE "-fPIC")
		set_property (TARGET ${ADAPTER_OBJECT} PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)
	endif ()

endif ()
