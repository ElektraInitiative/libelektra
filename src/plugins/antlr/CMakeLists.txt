include (LibAddMacros)

if (DEPENDENCY_PHASE)
	find_package (ANTLR4CPP QUIET)
	if (NOT ANTLR4CPP_FOUND)
		remove_plugin (antlr "ANTLR 4 CPP runtime (antlr4-cpp-runtime) not found")
	endif (NOT ANTLR4CPP_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (antlr
	CPP
	SOURCES
		antlr.hpp
		antlr.cpp
	ADD_TEST
	TEST_README
	INCLUDE_DIRECTORIES
		${ANTLR4CPP_INCLUDE_DIRS}
	LINK_LIBRARIES
		${ANTLR4CPP_LIBRARIES}
	)
