if (DEPENDENCY_PHASE)
	find_package(Iconv)

	if (NOT ICONV_FOUND)
		remove_plugin (iconv "Cannot find iconv library")
	endif ()
endif ()

add_plugin (iconv
	SOURCES
		conv.h
		iconv.c
	INCLUDE_DIRECTORIES
		${ICONV_INCLUDE_DIR}	
	LINK_LIBRARIES
		${ICONV_LIBRARIES}
	ADD_TEST
)
