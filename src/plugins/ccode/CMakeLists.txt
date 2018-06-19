if (ADDTESTING_PHASE)
	add_plugintest (ccode CPP LINK_ELEKTRA elektra-kdb elektra-plugin)
endif (ADDTESTING_PHASE)

add_plugin (ccode CPP SOURCES ccode.hpp ccode.cpp coder.hpp coder.cpp TEST_README TEST_REQUIRED_PLUGINS tcl base64)
