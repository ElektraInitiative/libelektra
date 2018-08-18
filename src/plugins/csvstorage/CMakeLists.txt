include (LibAddMacros)

add_plugin (csvstorage
	    SOURCES csvstorage.h
		    csvstorage.c
	    LINK_ELEKTRA elektra-ease
			 elektra-proposal
	    ADD_TEST
	    INSTALL_TEST_DATA
	    TEST_README)
