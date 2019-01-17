if (DEPENDENCY_PHASE)
	find_package (PEGTL 2.7.1 QUIET EXACT)
	if (NOT PEGTL_FOUND)
		remove_plugin (yaypeg "PEGTL (tao-pegtl == 2.7.1) not found")
	endif (NOT PEGTL_FOUND)
endif (DEPENDENCY_PHASE)

add_plugin (yaypeg
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    TEST_REQUIRED_PLUGINS directoryvalue
				  sync
				  yamlsmith
	    SOURCES state.cpp
		    parser.hpp
		    listener.hpp
		    listener.cpp
		    walk.hpp
		    walk.cpp
		    convert.hpp
		    convert.cpp
		    yaypeg.hpp
		    yaypeg.cpp)
