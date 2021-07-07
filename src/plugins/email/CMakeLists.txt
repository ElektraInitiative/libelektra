include (LibAddMacros)

add_plugin (
	email
	SOURCES email.h email.c test_email.h
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental
	TEST_REQUIRED_PLUGINS dump)
