include(LibAddMacros)

add_plugin(hosts
	SOURCES
		hosts.h
		hosts.c
		keymetaformatting.h
		keymetaformatting.c
	)

install(DIRECTORY hosts DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest(hosts)
