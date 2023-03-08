include (LibAddMacros)

add_plugin (
	logchange
	SOURCES logchange.h logchange.c
	LINK_ELEKTRA elektra-kdb
	ADD_TEST
	COMPONENT libelektra${SO_VERSION}-extra)
