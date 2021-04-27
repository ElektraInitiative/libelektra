include (LibAddMacros)

add_plugin (
	logchange
	SOURCES logchange.h logchange.c COMPONENT libelektra${SO_VERSION}-extra)
