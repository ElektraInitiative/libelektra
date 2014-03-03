include (LibAddMacros)

set (SOURCES type.hpp type.cpp
		types.hpp types.cpp
		type_checker.hpp)
add_sources (elektra-full ${SOURCES})
add_headers (SOURCES)
add_cppheaders (SOURCES)

set (PLUGIN_NAME elektra-type)

add_library (${PLUGIN_NAME} MODULE ${SOURCES})
target_link_libraries (${PLUGIN_NAME} elektra)
install (TARGETS ${PLUGIN_NAME}
	DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

#first cpp plugin test

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

#this test case works for full and static builds
#it will not relink correctly
file (GLOB TESTS testmod_*.cpp)
macro (do_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
	set (TEST_SOURCES ${HDR_FILES} ${source}.cpp ${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests/tests.cpp)
	add_executable (${source} ${TEST_SOURCES})

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
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_test (${name})
endforeach (file ${TESTS})

#lets use only the shared library because there we do not
#have symbol clashes
if (BUILD_SHARED)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
	file (GLOB UNIT_TEST_SOURCES testmod_type.cpp)
	add_executable(test_types
			${UNIT_TEST_SOURCES}
			${SOURCES}
			${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests/tests.cpp)
	add_test(test_types "${CMAKE_CURRENT_BINARY_DIR}/test_types")
	target_link_libraries (test_types elektra)
endif (BUILD_SHARED)
