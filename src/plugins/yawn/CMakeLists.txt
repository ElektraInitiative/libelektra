if (DEPENDENCY_PHASE)
	find_package (YAEP QUIET)
	if (NOT YAEP_FOUND)
		remove_plugin (yawn "YAEP could not be found")
	endif (NOT YAEP_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (yawn
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES yawn.hpp
		    yawn.cpp
	    INCLUDE_DIRECTORIES ${YAEP_INCLUDE_DIR}
	    LINK_LIBRARIES ${YAEP_LIBRARIES_CPP})
