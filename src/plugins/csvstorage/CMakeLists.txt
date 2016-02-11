include (LibAddMacros)

add_plugin(csvstorage
	SOURCES
		csvstorage.h
		csvstorage.c
	LINK_ELEKTRA
		elektra-ease
	)

install(DIRECTORY csvstorage DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest(csvstorage)
