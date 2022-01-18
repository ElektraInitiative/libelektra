include (LibAddMacros)

add_plugin (
	hexcode
	SOURCES hexcode.h hexcode.c
	ADD_TEST COMPONENT libelektra${SO_VERSION}-extra)
