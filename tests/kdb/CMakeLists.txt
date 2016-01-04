file (GLOB TESTS testkdb_*.cpp)
find_package(Threads)

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders(HDR_FILES)

include (LibAddTest)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} KDBTESTS LINK_TOOLS)
endforeach (file ${TESTS})

