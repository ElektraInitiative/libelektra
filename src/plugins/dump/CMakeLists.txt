include (LibAddPlugin)

add_plugin (dump SOURCES dump.hpp dump.cpp)

if (ADDTESTING_PHASE)
	add_plugintest (dump INSTALL_TEST_DATA INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR})
endif ()
