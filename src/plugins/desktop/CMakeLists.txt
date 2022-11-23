include (LibAddMacros)

add_plugin (
	desktop
	SOURCES desktop.h desktop.c
	ADD_TEST COMPONENT libelektra${SO_VERSION}-extra)
