add_plugin (ctype
	    SOURCES ctype.h
		    ctype.c
		    ctypes.h
		    ctypes.c
	    LINK_ELEKTRA elektra-highlevel
			 elektra-meta
			 elektra-ease
	    ADD_TEST
	    TEST_README)
