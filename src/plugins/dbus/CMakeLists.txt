find_package(DBus REQUIRED)
if (DBUS_FOUND)
	include (LibAddMacros)

	add_includes (elektra-full ${DBUS_INCLUDE_DIR})
	add_includes (elektra-full ${DBUS_ARCH_INCLUDE_DIR})
	include_directories (${DBUS_INCLUDE_DIR})
	include_directories (${DBUS_ARCH_INCLUDE_DIR})

	set (SOURCES dbus.h dbus.c sendmessage.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	add_library (elektra-dbus MODULE ${SOURCES})
	install (TARGETS elektra-dbus DESTINATION lib/elektra)

	target_link_libraries (elektra-dbus ${DBUS_LIBRARIES})
	add_libraries(elektra-full ${DBUS_LIBRARIES})

	add_plugintest (dbus)
else (DBUS_FOUND)
	remove_backend (dbus "dbus not found")
endif (DBUS_FOUND)
