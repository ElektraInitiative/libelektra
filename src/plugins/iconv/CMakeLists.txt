if (DEPENDENCY_PHASE)
	find_package (Iconv QUIET)

	if (NOT Iconv_FOUND)
		remove_plugin (iconv "Cannot find iconv library")
	endif ()
endif ()

add_plugin (iconv
	    SOURCES conv.h iconv.c
	    INCLUDE_DIRECTORIES ${Iconv_INCLUDE_DIRS}
	    LINK_LIBRARIES ${Iconv_LIBRARIES}
	    ADD_TEST TEST_README)
