include (LibAddMacros)

if (DEPENDENCY_PHASE)
	find_package (ODBC REQUIRED)
	if (NOT ODBC_FOUND)
		remove_plugin (backend_odbc "ODBC library files not found, make sure an ODBC variant (e.g. unixODBC) is installed")
	endif ()
endif ()


add_plugin (
	backend_odbc
	SOURCES backend_odbc.h backend_odbc.c backendprivate_odbc.h helpers_odbc.c
	INCLUDE_DIRECTORIES ${ODBC_INCLUDE_DIRS}
	LINK_LIBRARIES ${ODBC_LIBRARIES}
	ADD_TEST COMPONENT libelektra${SO_VERSION})
