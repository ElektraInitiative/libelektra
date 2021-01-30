include (LibAddPlugin)

list (FIND ADDED_PLUGINS "internalnotification" FOUND_NAME)
if (FOUND_NAME EQUAL -1)
	message (STATUS "Exclude Library notification because internalnotification plugin excluded")
else ()
	set (SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/notification.c")

	set (LIBRARY_NAME elektra-notification)

	add_lib (
		notification
		SOURCES
		${SOURCES}
		LINK_ELEKTRA
		elektra-kdb
		elektra-ease
		elektra-invoke
		COMPONENT
		libelektra${SO_VERSION})

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc.in" "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc" @ONLY)

	install (
		FILES "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc"
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
		COMPONENT libelektra-dev)

	if (ENABLE_TESTING)
		add_subdirectory (tests)
	endif (ENABLE_TESTING)

endif ()
