include (LibAddMacros)

add_plugin (lock
	SOURCES
		lock.h
		lock.c
	)

add_plugintest (lock)
