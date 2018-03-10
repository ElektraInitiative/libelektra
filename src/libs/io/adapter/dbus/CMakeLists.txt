include (LibAddMacros)

# disable adapter when compiling with musl
if (${CMAKE_C_COMPILER} MATCHES "musl")
	message (WARNING "D-Bus I/O adapter disabled when compiling with musl. Targets that depend on this adapter will fail to compile.")
else ()
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
			set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES" "${DBUS_LIBRARIES}")
		endif ()
		if (BUILD_SHARED)
			target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})
		endif ()

	endif ()
endif ()
