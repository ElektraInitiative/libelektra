if (DEPENDENCY_PHASE)
	find_package (DBus QUIET)

	if (NOT DBUS_FOUND)
		remove_plugin (dbus "dbus package not found")
	endif ()
endif ()

add_plugin (
	dbus
	SOURCES dbus.h dbus.c sendmessage.c
	INCLUDE_DIRECTORIES ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR}
	LINK_LIBRARIES ${DBUS_LIBRARIES} LINK_ELEKTRA elektra-kdb COMPONENT libelektra${SO_VERSION}-dbus)

add_plugintest (dbus testmod_dbus.c receivemessage.c INCLUDE_DIRECTORIES ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})

if (ADDTESTING_PHASE)
	set (example example_dbus)

	set (SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/example_dbus.c ${CMAKE_CURRENT_SOURCE_DIR}/sendmessage.c
		       ${CMAKE_CURRENT_SOURCE_DIR}/receivemessage.c)
	set (HDR_FILES ${CMAKE_CURRENT_SOURCE_DIR}/dbus.h)
	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

	add_executable (${example} ${SOURCES})

	target_include_directories (${example} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})

	target_link_libraries (${example} ${DBUS_LIBRARIES})
	target_link_elektra (${example} elektra-kdb)
endif ()
