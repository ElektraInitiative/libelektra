include (LibAddMacros)

add_plugin (
	recorder
	SOURCES recorder.h recorder.c
	ADD_TEST
	LINK_ELEKTRA elektra-kdb elektra-record
	COMPONENT libelektra${SO_VERSION})
