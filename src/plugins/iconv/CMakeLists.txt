include (LibAddMacros)

add_plugin(iconv
	SOURCES
		iconv.h
		iconv.c
	)
install(DIRECTORY iconv DESTINATION ${TARGET_TEST_DATA_FOLDER})
add_plugintest (iconv)
