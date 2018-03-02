include (LibAddMacros)

find_package (DBus)

if (DBUS_FOUND)

	set (ADAPTER_NAME dbus)

	## Build library
	set (ADAPTER_SRC_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/dbus.c"
	)

	set (SOURCES ${ADAPTER_SRC_FILES} ${ADAPTER_HDR_FILES})

	set (ADAPTER_LIBRARY elektra-io-adapter-${ADAPTER_NAME})

	add_lib (io-adapter-${ADAPTER_NAME}
		SOURCES ${SOURCES}
		LINK_ELEKTRA
			elektra-io
		LINK_LIBRARIES
			${DBUS_LIBRARIES}
	)

	if (BUILD_FULL OR BUILD_STATIC)
		add_includes ("elektra-full" "${DBUS_INCLUDE_DIR};${DBUS_ARCH_INCLUDE_DIR}")
	endif ()
	if (BUILD_SHARED)
		target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})
	endif ()

endif ()
