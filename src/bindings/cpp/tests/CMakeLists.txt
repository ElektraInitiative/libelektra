include (LibAddTest)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

set (CLANG_7_OR_LATER
     ${CMAKE_CXX_COMPILER_ID}
     STREQUAL
     "Clang"
     AND
     NOT
     (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7))

set (TESTCPP_CONTEXTUAL_BASIC_PROPERTIES "-Wno-gnu-zero-variadic-macro-arguments")
if (${CLANG_7_OR_LATER})
	string (APPEND TESTCPP_CONTEXTUAL_BASIC_PROPERTIES " -Wno-self-assign-overloaded")
endif (${CLANG_7_OR_LATER})
set_source_files_properties (testcpp_contextual_basic.cpp
			     PROPERTIES
			     COMPILE_FLAGS
			     ${TESTCPP_CONTEXTUAL_BASIC_PROPERTIES})

file (GLOB TESTS testcpp_*.cpp)
foreach (file ${TESTS})

	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_ELEKTRA elektra-kdb elektra-meta)
endforeach (file ${TESTS})

set_property (TEST testcpp_kdb PROPERTY LABELS kdbtests)
