include (LibAddMacros)

file (GLOB INIH_FILES ${CMAKE_CURRENT_SOURCE_DIR}/inih-r29/*)

add_plugin(ini
	SOURCES
		ini.h
		ini.c
		${INIH_FILES}
	INCLUDE_DIRECTORIES
		${CMAKE_CURRENT_SOURCE_DIR}/inih-r29
	LINK_ELEKTRA
		elektra-ease
		elektra-meta
		elektra-proposal
	COMPILE_DEFINITIONS
		INI_ALLOW_MULTILINE=0
	ADD_TEST
	INSTALL_TEST_DATA
	)
