include (LibAddMacros)

add_plugin (
	syslog
	SOURCES log.h syslog.c COMPONENT libelektra${SO_VERSION}-extra)
