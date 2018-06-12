include (LibAddMacros)

add_plugin (ccode SOURCES ccode.h ccode.c ADD_TEST TEST_README TEST_REQUIRED_PLUGINS tcl base64)
