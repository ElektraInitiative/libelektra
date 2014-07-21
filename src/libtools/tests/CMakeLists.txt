include(LibAddMacros)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders(HDR_FILES)

add_subdirectory(${GOOGLETEST_ROOT} ${CMAKE_CURRENT_BINARY_DIR}/gtest)
include_directories(${GOOGLETEST_ROOT}/include ${GOOGLETEST_ROOT})


macro (do_tool_test source)
	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
	include_directories ("${PROJECT_SOURCE_DIR}/src/bindings/cpp/tests/")
	set (SOURCES ${HDR_FILES} ${source}.cpp
		${PROJECT_SOURCE_DIR}/src/bindings/cpp/tests/tests.cpp
		${PROJECT_SOURCE_DIR}/src/bindings/cpp/tests/tests.hpp)
	add_executable (${source} ${SOURCES})

	if (BUILD_FULL)
		target_link_libraries (${source} elektratools-full)
	else (BUILD_FULL)
		target_link_libraries (${source} elektratools-static)
	endif (BUILD_FULL)

	target_link_libraries(${name} gtest gtest_main)

	if (INSTALL_TESTING)
		install (TARGETS ${source}
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
	endif (INSTALL_TESTING)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	add_test (${source}
			"${CMAKE_CURRENT_BINARY_DIR}/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}/"
			)
endmacro (do_tool_test)


file (GLOB TESTS testtool_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	do_tool_test (${name})
endforeach (file ${TESTS})
