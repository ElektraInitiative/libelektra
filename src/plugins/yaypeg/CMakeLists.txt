include (LibAddMacros)

add_plugin (yaypeg
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES yaypeg.hpp
		    yaypeg.cpp
		    yaypeg_delegate.hpp
		    yaypeg_delegate.cpp)
