find_package (DBus QUIET)

if (DBUS_FOUND)

	set (ADAPTER_OBJECT io-adapter-dbus)

	set (ADAPTER_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/dbus.c")

	set (SOURCES ${ADAPTER_SRC_FILES} ${ADAPTER_HDR_FILES})

	add_library (${ADAPTER_OBJECT} OBJECT ${SOURCES})
	target_include_directories (${ADAPTER_OBJECT} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})

	# NOTE CMAKE_POSITION_INDEPENDENT_CODE does not seem to work (e.g. CMake 3.9.2 on Travis)
	target_compile_options (${ADAPTER_OBJECT} PRIVATE "-fPIC")
	set_property (TARGET ${ADAPTER_OBJECT} PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)

endif ()
