include(LibAddMacros)

add_headers (HDR_FILES)

include (LibAddTest)

file (GLOB TESTS test_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} TARGET_LINK_LIBRARIES elektragetenv)
endforeach (file ${TESTS})
