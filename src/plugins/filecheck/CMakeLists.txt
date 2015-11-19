include (LibAddMacros)

add_plugin (filecheck
	SOURCES
		filecheck.h
		filecheck.c
	)
install(DIRECTORY filecheck DESTINATION ${TARGET_TEST_DATA_FOLDER})
add_plugintest (filecheck)
