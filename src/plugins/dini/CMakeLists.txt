include (LibAddMacros)

add_plugin (dini
	    SOURCES dini.h dini.c
	    LINK_ELEKTRA elektra-invoke
	    TEST_README)
