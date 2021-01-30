include (LibAddMacros)

add_plugin (
	internalnotification
	SOURCES internalnotification.h internalnotification.c
	ADD_TEST
	LINK_ELEKTRA elektra-kdb COMPONENT libelektra${SO_VERSION})
