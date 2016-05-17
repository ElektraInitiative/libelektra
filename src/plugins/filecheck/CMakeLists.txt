include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (Iconv)

	if (NOT ICONV_FOUND)
		remove_plugin(filecheck "Cannot find iconv library")
	endif ()
endif ()

add_plugin (filecheck
	SOURCES
		filecheck.h
		filecheck.c
	INCLUDE_DIRECTORIES
		${ICONV_INCLUDE_DIR}	
	LINK_LIBRARIES
		${ICONV_LIBRARIES}
	ADD_TEST
	INSTALL_TESTING
)
