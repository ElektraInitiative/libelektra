if (ADDTESTING_PHASE)
	add_plugintest (type CPP MEMLEAK LINK_ELEKTRA elektra-kdb elektra-plugin)
endif (ADDTESTING_PHASE)

add_plugin (type CPP SOURCES type.hpp type.cpp types.hpp types.cpp type_checker.hpp TEST_README)
