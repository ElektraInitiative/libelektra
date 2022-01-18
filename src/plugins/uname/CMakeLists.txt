if (DEPENDENCY_PHASE)
	try_compile (HAS_UNAME "${CMAKE_BINARY_DIR}" "${PROJECT_SOURCE_DIR}/src/plugins/uname/testuname.c")

	include (LibAddMacros)

	if (NOT HAS_UNAME)
		remove_plugin (uname "uname is missing")
	endif ()
endif ()

add_plugin (
	uname
	SOURCES uname.h uname.c
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS noresolver COMPONENT libelektra${SO_VERSION}-extra)
