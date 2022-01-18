include (LibAddMacros)

add_plugin (
	tracer
	SOURCES tracer.h tracer.c COMPONENT libelektra${SO_VERSION}-extra)
