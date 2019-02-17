include (LibAddMacros)

add_plugin (quickdump
	    SOURCES quickdump.h
		    quickdump.c
		    portable/pendian.h
		    portable/pendian_detect.h
	    TEST_README)

if (ADDTESTING_PHASE)
	add_plugintest (quickdump INSTALL_TEST_DATA INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
endif ()
