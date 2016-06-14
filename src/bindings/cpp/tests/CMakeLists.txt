include(LibAddTest)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

file (GLOB TESTS testcpp_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_ELEKTRA elektra-kdb elektra-meta)
endforeach (file ${TESTS})

set_property(TEST testcpp_kdb
	PROPERTY LABELS kdbtests)
