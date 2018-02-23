if (DEPENDENCY_PHASE)
	find_package (DBus)

	if (NOT DBUS_FOUND)
		remove_plugin (dbus "dbus package not found")
	endif ()
endif ()

add_plugin (dbus
	SOURCES
		dbus.h dbus.c
		sendmessage.c receivemessage.c
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

add_plugintest (dbus
	INCLUDE_DIRECTORIES
		${DBUS_INCLUDE_DIR}
		${DBUS_ARCH_INCLUDE_DIR}
	)
