include (LibAddMacros)

add_plugin(lineendings
	SOURCES
		lineendings.h
		lineendings.c
	)
install(DIRECTORY lineendings DESTINATION ${TARGET_TEST_DATA_FOLDER})
add_plugintest(lineendings)
