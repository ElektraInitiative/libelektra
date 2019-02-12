add_plugin (ctype
	    ADD_TEST
	    SOURCES ctype.h
		    ctype.c
		    ctypes.h
		    ctypes.c
	    LINK_ELEKTRA elektra-highlevel
			 elektra-meta
	    TEST_README)
