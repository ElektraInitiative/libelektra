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
	)

add_plugintest (ini)
