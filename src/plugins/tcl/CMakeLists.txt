if (DEPENDENCY_PHASE)
	find_package (Boost QUIET)
	if (NOT Boost_FOUND)
		remove_plugin (tcl "boost not found (please install libboost-dev)")
	endif ()
endif ()

add_plugin (
	tcl CPP
	INCLUDE_SYSTEM_DIRECTORIES ${Boost_INCLUDE_DIRS}
	SOURCES tcl.hpp tcl.cpp action.hpp action.cpp printer.hpp printer.cpp
	LINK_ELEKTRA elektra-ease
	TEST_README)
