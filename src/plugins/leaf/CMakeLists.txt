include (LibAddMacros)

add_plugin (leaf
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES leaf.hpp
		    leaf.cpp
		    leaf_delegate.hpp
		    leaf_delegate.cpp
	    LINK_ELEKTRA elektra-ease)
