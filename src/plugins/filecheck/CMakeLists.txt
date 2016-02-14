include (LibAddMacros)

find_package(Iconv)

if (ICONV_FOUND)
	add_plugin (filecheck
		SOURCES
			filecheck.h
			filecheck.c
		INCLUDE_DIRECTORIES
			${ICONV_INCLUDE_DIR}	
		LINK_LIBRARIES
			${ICONV_LIBRARIES}
	)
	install(DIRECTORY filecheck DESTINATION ${TARGET_TEST_DATA_FOLDER})
	add_plugintest (filecheck)
else ()
	remove_plugin(filecheck "Can't find iconv library")
endif ()
