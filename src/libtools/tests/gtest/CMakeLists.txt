add_subdirectory (${CMAKE_SOURCE_DIR}/lib/gtest-1.7.0 ${CMAKE_BINARY_DIR}/lib/gtest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

file (GLOB TESTS *.cpp)
foreach (file ${TESTS})

	get_filename_component (name ${file} NAME_WE)
	
	do_test (${name})
	
	target_link_libraries(${name} gtest gtest_main)

endforeach(file ${TESTS})
