include(LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	set (SOURCES ${HDR_FILES} ${source}.cpp tests.cpp tests.hpp)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	add_test (${source}
			"${CMAKE_CURRENT_BINARY_DIR}/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}/"
			)
endmacro (do_test)


file (GLOB TESTS testcpp_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})

if (COMPILE_CXX11_MODE)
	file (GLOB TESTS testcpp11_*.cpp)
	foreach (file ${TESTS})
		get_filename_component (name ${file} NAME_WE)
		do_test (${name})
	endforeach (file ${TESTS})
endif (COMPILE_CXX11_MODE)
