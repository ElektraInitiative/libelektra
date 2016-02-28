find_package (Boost)
if (Boost_FOUND)
	add_plugin(tcl
		CPP
		SOURCES
			tcl.hpp tcl.cpp
			action.hpp action.cpp
			printer.hpp printer.cpp
		)
else (Boost_FOUND)
	remove_plugin (tcl "boost not found")
endif (Boost_FOUND)
