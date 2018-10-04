include (LibAddTest)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

set (CLANG_7_OR_LATER ${CMAKE_CXX_COMPILER_ID} MATCHES "Clang" AND NOT (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7))

file (GLOB TESTS
	   testcpp_*.cpp)
foreach (file ${TESTS})

	if (file MATCHES testcpp_contextual_basic.cpp AND ${CLANG_7_OR_LATER})
		set_source_files_properties (${file} PROPERTIES COMPILE_FLAGS "-Wno-self-assign-overloaded")
	endif (file MATCHES testcpp_contextual_basic.cpp AND ${CLANG_7_OR_LATER})

	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name}
		   LINK_ELEKTRA elektra-kdb
				elektra-meta)
endforeach (file ${TESTS})

set_property (TEST testcpp_kdb
	      PROPERTY LABELS
		       kdbtests)
