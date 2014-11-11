include(LibAddMacros)

add_plugin(hosts
	SOURCES
		hosts.h
		hosts.c
	)

install(DIRECTORY hosts DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest(hosts)
