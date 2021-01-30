include (LibAddMacros)

add_plugin (
	template
	SOURCES template.h template.c
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)
