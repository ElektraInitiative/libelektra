include (LibAddBinding)

if (DEPENDENCY_PHASE)
	find_package (DBus)

	if (NOT DBUS_FOUND)
		remove_plugin (dbusrecv "dbus package not found")
	endif ()
endif ()

add_plugin (dbusrecv
	SOURCES
		dbusrecv.h dbusrecv.c
		receivemessage.c
	INCLUDE_DIRECTORIES
		${DBUS_INCLUDE_DIR}
		${DBUS_ARCH_INCLUDE_DIR}
	LINK_ELEKTRA
		elektra-io-adapter-dbus
	LINK_LIBRARIES
		${DBUS_LIBRARIES}
	DEPENDS
		elektra-io-adapter-dbus
	)

if (ADDTESTING_PHASE)
	# the test requires an IO binding; using io_uv for now
	check_binding_was_added ("io_uv" IS_INCLUDED)
	if (BUILD_TESTING AND IS_INCLUDED)
		add_plugintest (dbusrecv
			LINK_LIBRARIES
				${DBUS_LIBRARIES}
			INCLUDE_DIRECTORIES
				${DBUS_INCLUDE_DIR}
				${DBUS_ARCH_INCLUDE_DIR}
			LINK_ELEKTRA
				elektra-io-adapter-dbus
				elektra-io
				elektra-io-uv
			)
	else ()
		message (WARNING "io_uv bindings are required for testing, test deactivated")
	endif ()
endif ()
