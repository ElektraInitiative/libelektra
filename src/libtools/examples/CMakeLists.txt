include(LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

macro (do_example source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
endmacro (do_example)

file (GLOB TESTS *.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_example (${name})
endforeach (file ${TESTS})
