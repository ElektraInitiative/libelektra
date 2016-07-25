include (LibAddMacros)

#don't call add_headers in a loop
add_headers (HDR_FILES)

macro (do_example source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.c)
	add_executable (${source} ${SOURCES})
	add_dependencies (${source} kdberrors_generated)

	target_link_elektra(${source} elektra-kdb)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

file (GLOB TESTS *.c)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_example (${name})
endforeach (file ${TESTS})

target_link_elektra(cascading elektra-kdb)
target_link_elektra(cut elektra-kdb)
target_link_elektra(kdbget elektra-kdb)
target_link_elektra(kdbget_error elektra-kdb)
target_link_elektra(kdbintro elektra-kdb)
target_link_elektra(kdbopen elektra-kdb)
target_link_elektra(kdbset elektra-kdb)
target_link_elektra(set elektra-kdb)
