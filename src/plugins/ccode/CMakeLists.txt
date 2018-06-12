include (LibAddMacros)

add_plugin (ccode CPP SOURCES ccode.hpp ccode.cpp ADD_TEST TEST_README TEST_REQUIRED_PLUGINS tcl base64)
