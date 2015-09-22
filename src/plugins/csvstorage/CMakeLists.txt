include (LibAddMacros)

add_plugin(csvstorage
	SOURCES
		csvstorage.h
		csvstorage.c
	)

install(DIRECTORY csvstorage DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest(csvstorage)
