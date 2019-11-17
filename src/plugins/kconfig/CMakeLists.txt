include (LibAddMacros)

add_plugin (kconfig
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES kconfig.hpp
		    kconfig.cpp
		    kconfig_delegate.hpp
		    kconfig_delegate.cpp)
