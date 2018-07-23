include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (strdup "string.h" HAVE_STRDUP)
check_symbol_exists (strtok_r "string.h" HAVE_STRTOK_R)
check_symbol_exists (strcasecmp "strings.h" HAVE_STRCASECMP)

if (NOT (HAVE_STRDUP AND HAVE_STRTOK_R AND HAVE_STRCASECMP))
	remove_plugin(ini "requires strdup, strtok_r and strcasecmp")
endif ()

file (GLOB INIH_FILES ${CMAKE_CURRENT_SOURCE_DIR}/inih-r29/*)

add_plugin (ini
	    SOURCES ini.h
		    ini.c
		    ${INIH_FILES}
	    INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/inih-r29
	    LINK_ELEKTRA elektra-ease
			 elektra-meta
			 elektra-proposal
	    COMPILE_DEFINITIONS INI_ALLOW_MULTILINE=0
	    ADD_TEST
	    INSTALL_TEST_DATA
	    TEST_README)
