include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	find_package (OpenLdap QUIET)
	if (NOT OPENLDAP_FOUND)
		remove_plugin (ldif "Cannot find openldap library")
	endif ()
endif ()

add_plugin (
	ldif
	SOURCES ldif.h ldif.c
	INCLUDE_DIRECTORIES ${OPENLDAP_INCLUDE_DIR}
	LINK_ELEKTRA elektra-ease
	LINK_LIBRARIES ${OPENLDAP_LIBRARIES}
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
