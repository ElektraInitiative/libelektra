include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (Iconv QUIET)

	if (NOT Iconv_FOUND)
		remove_plugin (filecheck "Cannot find iconv library")
	endif ()
endif ()

add_plugin (filecheck
	    SOURCES filecheck.h filecheck.c
	    INCLUDE_DIRECTORIES ${Iconv_INCLUDE_DIRS}
	    LINK_LIBRARIES ${Iconv_LIBRARIES}
	    ADD_TEST INSTALL_TEST_DATA)
