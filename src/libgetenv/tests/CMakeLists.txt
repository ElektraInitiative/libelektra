include(LibAddMacros)

add_headers (HDR_FILES)

include (LibAddTest)

file (GLOB TESTS test_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name})
	target_link_libraries(${name} elektragetenv)
endforeach (file ${TESTS})
