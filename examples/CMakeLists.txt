#project (examples)

#find_package (Elektra)

#if (ELEKTRA_FOUND)
#	include (${Elektra_USE_FILE})
#	add_executable (hello hello.c)
#	target_link_libraries (hello elektra)
#endif (ELEKTRA_FOUND)


include (LibAddMacros)

#dont call add_headers in a loop
add_headers (HDR_FILES)

macro (do_example source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

do_example (meta)
do_example (get)
