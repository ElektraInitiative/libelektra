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
	INCLUDE_DIRECTORIES
		${NI}/include
		${NI}/src/include
	)

include_directories(${NI}/include)
file (GLOB NI_INI_FILES ${NI}/src/tests/*.ini)
copy_file (${NI_INI_FILES} "${CMAKE_CURRENT_BINARY_DIR}")

# testmod_ni mainly tests nickel functions so we need to
# link against nickel objects
add_plugintest (ni ${NI_FILES}
	INCLUDE_DIRECTORIES
		${NI}/include
		${NI}/src/include)
