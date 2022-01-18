include (LibAddMacros)

add_plugin (
	rgbcolor
	SOURCES rgbcolor.h rgbcolor.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-extra)
