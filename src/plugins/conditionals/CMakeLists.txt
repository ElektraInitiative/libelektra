include (LibAddMacros)

add_plugin (conditionals
	    SOURCES conditionals.h conditionals.c
	    LINK_ELEKTRA elektra-meta
	    ADD_TEST TEST_README
	    TEST_REQUIRED_PLUGINS dump ini ni)
