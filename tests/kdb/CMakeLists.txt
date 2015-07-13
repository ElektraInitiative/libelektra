file (GLOB TESTS testkdb_*.cpp)
find_package(Threads)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders(HDR_FILES)

include (LibAddTest)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name})
	set_property(TEST ${name} PROPERTY LABELS kdbtests)
	target_link_libraries(${name} ${CMAKE_THREAD_LIBS_INIT})
endforeach (file ${TESTS})

