include (LibAddMacros)

add_plugin (
	timeofday
	SOURCES timeofday.h timeofday.c COMPONENT libelektra${SO_VERSION}-extra)
