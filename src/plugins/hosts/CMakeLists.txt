include(LibAddMacros)

add_plugin(hosts
	SOURCES
		hosts.h
		hosts-get.c
		hosts-set.c
		keymetaformatting.h
		keymetaformatting.c
	LINK_ELEKTRA
		elektra-ease
		elektra-proposal
		elektra-meta
	)

install(DIRECTORY hosts DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest(hosts)
