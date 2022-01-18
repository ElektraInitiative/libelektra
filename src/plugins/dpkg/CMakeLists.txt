include (LibAddMacros)

add_plugin (
	dpkg
	SOURCES dpkg.h dpkg.c COMPONENT libelektra${SO_VERSION}-extra)
