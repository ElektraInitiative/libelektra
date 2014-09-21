include(LibAddMacros)

add_plugin(hosts
	SOURCES
		hosts.h
		hosts.c
	)

add_plugintest(hosts)
