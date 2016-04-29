include (LibAddMacros)

file (GLOB INIH_FILES ${CMAKE_CURRENT_SOURCE_DIR}/inih-r29/*)
set (INIH_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/inih-r29)
add_definitions( -DINI_ALLOW_MULTILINE=0 )

add_plugin(ini
	SOURCES
		ini.h
		ini.c
		${INIH_FILES}
	INCLUDE_DIRECTORIES
		${INIH_INCLUDE}
	LINK_ELEKTRA
		elektra-ease
		elektra-meta
		elektra-proposal
	)

install (DIRECTORY ini DESTINATION ${TARGET_TEST_DATA_FOLDER})

add_plugintest (ini)
