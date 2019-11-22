include (LibAddMacros)

set (TEST_ARGUMENTS ADD_TEST CPP_TEST INSTALL_TEST_DATA)

add_plugin (kconfig
	    CPP
	    ${TEST_ARGUMENTS}
	    TEST_README
	    SOURCES kconfig.hpp
		    kconfig.cpp
		    kconfig_delegate.hpp
		    kconfig_delegate.cpp
		    kconfig_parser.cpp
		    kconfig_parser_exception.cpp
		    file_utility.cpp)
