if (DEPENDENCY_PHASE)
	find_package (Boost)
	if (NOT Boost_FOUND)
		remove_plugin (tcl "boost not found, but needed for tcl plugin")
	endif ()
endif ()

add_plugin(tcl
	CPP
	INCLUDE_DIRECTORIES
		${Boost_INCLUDE_DIR}
	SOURCES
		tcl.hpp tcl.cpp
		action.hpp action.cpp
		printer.hpp printer.cpp
	)
