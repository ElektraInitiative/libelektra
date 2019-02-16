include (LibAddMacros)

add_plugin (quickdump
	    SOURCES quickdump.h
		    quickdump.c
		    portable/pendian.h
		    portable/pendian_detect.h
		    test.quickdump.h
	    ADD_TEST
	    TEST_README)
