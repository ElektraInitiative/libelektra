include (LibAddMacros)

add_plugin (
	syslog
	SOURCES log.h syslog.c
	LINK_ELEKTRA elektra-kdb
	ADD_TEST
	COMPONENT libelektra${SO_VERSION}-extra)
