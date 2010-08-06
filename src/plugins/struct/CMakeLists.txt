include (LibAddMacros)

set (SOURCES struct.hpp struct.cpp checker.cpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

add_library (elektra-struct MODULE ${SOURCES})
install (TARGETS elektra-struct DESTINATION lib/elektra)

#first cpp plugin test

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
	set (SOURCES ${HDR_FILES} ${source}.cpp ${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests/tests.cpp)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektra-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektra-static)
	endif (BUILD_FULL)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_CONFIG_H)
	add_test (${source}
			"${CMAKE_CURRENT_BINARY_DIR}/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}/"
			)
endmacro (do_test)


file (GLOB TESTS testmod_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})
