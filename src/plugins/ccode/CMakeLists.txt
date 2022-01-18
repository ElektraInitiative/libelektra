add_plugin (
	ccode CPP ADD_TEST CPP_TEST
	SOURCES ccode.hpp ccode.cpp coder.hpp coder.cpp
	TEST_README
	TEST_REQUIRED_PLUGINS tcl base64 COMPONENT libelektra${SO_VERSION}-extra)
