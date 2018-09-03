include (LibAddMacros)

if (DEPENDENCY_PHASE)
	if (APPLE)
		# Add Homebrew path to Homebrew version of Bison
		list (APPEND CMAKE_PREFIX_PATH
			     "/usr/local/opt/bison")
	endif (APPLE)
	find_package (Bison 3 QUIET)
	if (NOT BISON_FOUND)
		remove_plugin (yambi "Bison 3 (bison) not found")
	endif (NOT BISON_FOUND)

	bison_target (YAMBI ${CMAKE_CURRENT_SOURCE_DIR}/parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp)

	if (NOT BISON_YAMBI_DEFINED)
		remove_plugin (yambi "Unable to generate parser")
	endif (NOT BISON_YAMBI_DEFINED)
endif (DEPENDENCY_PHASE)

add_plugin (yambi
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES yambi.hpp
		    yambi.cpp)
