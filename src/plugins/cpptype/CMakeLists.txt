# ~~~
# The unit test for the plugin fails on an ASAN enabled macOS build, if we do not disable container overflow detection.
# Reference: https://travis-ci.org/sanssecours/elektra/jobs/418554628
# ~~~
add_plugin (cpptype
	    ADD_TEST
	    CPP_TEST
	    CPP
	    SOURCES cpptype.hpp
		    cpptype.cpp
		    types.hpp
		    types.cpp
		    type_checker.hpp
	    TEST_README
	    TEST_ENVIRONMENT "ASAN_OPTIONS=detect_container_overflow=0")
