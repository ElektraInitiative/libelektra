include (LibAddMacros)

find_package(iconv)

if (ICONV_FOUND)
	add_plugin(iconv
		SOURCES
			conv.h
			iconv.c
		INCLUDE_DIRECTORIES
			${ICONV_INCLUDE_DIR}	
		LINK_LIBRARIES
			${ICONV_LIBRARIES}
	)
	add_plugintest (iconv)
else ()
	remove_plugin(iconv "Can't find iconv library")
endif ()
