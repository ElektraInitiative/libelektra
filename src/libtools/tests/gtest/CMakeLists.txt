add_subdirectory (${CMAKE_SOURCE_DIR}/lib/gtest-1.7.0 ${CMAKE_BINARY_DIR}/lib/gtest)
include_directories(SYSTEM ${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

set_property (TARGET gtest PROPERTY COMPILE_FLAGS "-w" )
set_property (TARGET gtest_main PROPERTY COMPILE_FLAGS "-w" )

file (GLOB TESTS *.cpp)
foreach (file ${TESTS})

	get_filename_component (name ${file} NAME_WE)
	
	do_tool_test (${name})

	target_link_libraries(${name} gtest gtest_main)

endforeach(file ${TESTS})
