find_package(DBus REQUIRED)

if (DBUS_FOUND)
	include (LibAddMacros)

	generate_readme (dbus)
	add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
	include_directories (${CMAKE_CURRENT_BINARY_DIR})

	add_includes (elektra-full ${DBUS_INCLUDE_DIR})
	add_includes (elektra-full ${DBUS_ARCH_INCLUDE_DIR})
	include_directories (${DBUS_INCLUDE_DIR})
	include_directories (${DBUS_ARCH_INCLUDE_DIR})

	set (SOURCES dbus.h dbus.c sendmessage.c receivemessage.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-dbus)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)
	target_link_libraries (${PLUGIN_NAME} ${DBUS_LIBRARIES})

	install (TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	add_libraries(elektra-full ${DBUS_LIBRARIES})

	add_plugintest (dbus)
else (DBUS_FOUND)
	remove_plugin (dbus "dbus not found")
endif (DBUS_FOUND)
