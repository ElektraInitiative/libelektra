include (LibAddMacros)

if (DEPENDENCY_PHASE)
	find_package (ODBC REQUIRED)
	if (NOT ODBC_FOUND)
		remove_plugin (backendODBC "ODBC library files not found, make sure an ODBC variant (e.g. unixODBC) is installed")
	endif ()
endif ()


add_plugin (
	backendOdbc
	SOURCES backendOdbc.h backendOdbc.c
	INCLUDE_DIRECTORIES ${ODBC_INCLUDE_DIRS}
	LINK_LIBRARIES ${ODBC_LIBRARIES}
	ADD_TEST COMPONENT libelektra${SO_VERSION})
