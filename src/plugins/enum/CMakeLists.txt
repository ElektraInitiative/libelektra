include (LibAddMacros)

add_plugin (enum
	    SOURCES enum.h enum.c
	    LINK_ELEKTRA elektra-meta elektra-utility
	    ADD_TEST TEST_README)
