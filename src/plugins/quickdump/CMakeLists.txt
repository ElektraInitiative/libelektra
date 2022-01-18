include (LibAddMacros)

add_plugin (
	quickdump
	SOURCES quickdump.h quickdump.c
	TEST_README COMPONENT libelektra${SO_VERSION})

if (ADDTESTING_PHASE)
	add_plugintest (quickdump INSTALL_TEST_DATA INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
endif ()
