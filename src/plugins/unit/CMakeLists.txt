include (LibAddMacros)

add_plugin (
	unit
	SOURCES unit.h unit.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-extra)
