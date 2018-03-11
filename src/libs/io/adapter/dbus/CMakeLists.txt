include (LibAddMacros)

find_package (DBus)

if (DBUS_FOUND)
	# check if platform can actually link against dbus
	# required for musl-gcc which cannot link agains dbus
	try_compile (CAN_LINK_DBUS
		"${CMAKE_BINARY_DIR}"
		"${CMAKE_CURRENT_SOURCE_DIR}/try_compile_dbus.c"
		CMAKE_FLAGS
			-DINCLUDE_DIRECTORIES:STRING=${DBUS_INCLUDE_DIR}\;${DBUS_ARCH_INCLUDE_DIR}
			-DLINK_LIBRARIES:PATH=${DBUS_LIBRARIES}
		)

	if (NOT CAN_LINK_DBUS)
		message (WARNING "D-Bus I/O adapter disabled since linking against D-Bus failed. Targets that depend on this adapter will fail to compile.")
	endif ()
endif ()

if (DBUS_FOUND AND CAN_LINK_DBUS)

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
		set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES" "${DBUS_LIBRARIES}")
	endif ()
	if (BUILD_SHARED)
		target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})
	endif ()

endif ()
