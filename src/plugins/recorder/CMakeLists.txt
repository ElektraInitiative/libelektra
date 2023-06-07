include (LibAddMacros)

add_plugin (
	recorder
	SOURCES recorder.h recorder.c
	ADD_TEST
	LINK_ELEKTRA elektra-record elektra-kdb COMPONENT libelektra${SO_VERSION})
