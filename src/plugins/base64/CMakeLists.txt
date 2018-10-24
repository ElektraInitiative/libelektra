include (LibAddPlugin)

# We disable the test on Linux, if only `BUILD_FULL` is enabled. Otherwise the compiler reports errors about undefined references to
# `base64Encode` and `base64Decode`. See also: https://travis-ci.org/sanssecours/elektra/jobs/445799127.
set (DISABLE_TEST ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${BUILD_FULL} AND NOT ${BUILD_SHARED})
if (${DISABLE_TEST})
	set (TEST_ARGUMENTS "")
else (${DISABLE_TEST})
	set (TEST_ARGUMENTS ADD_TEST)
endif (${DISABLE_TEST})

add_plugin (base64
	    SOURCES base64_functions.h
		    base64_functions.c
		    base64.h
		    base64.c
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"base64\"
				ELEKTRA_PLUGIN_NAME_C=base64
	    TEST_README
	    ${TEST_ARGUMENTS}
	    TEST_REQUIRED_PLUGINS ini)
