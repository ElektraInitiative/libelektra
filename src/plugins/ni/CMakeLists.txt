include (LibAddMacros)

set (NI ${CMAKE_CURRENT_SOURCE_DIR}/nickel-1.1.0)

file (GLOB NI_FILES
	${NI}/src/*.c
	${NI}/src/*.h
	)

add_plugin(ni
	SOURCES
		ni.h
		ni.c
		${NI_FILES}
	COMPILE_DEFINITIONS
		ELEKTRA_NO_META
	INCLUDE_DIRECTORIES
		${NI}/include
		${NI}/src/include
	)

include_directories(${NI}/include)
file (GLOB NI_INI_FILES ${NI}/src/tests/*.ini)
copy_file (${NI_INI_FILES} "${CMAKE_CURRENT_BINARY_DIR}")
add_plugintest (ni)
