include (LibAddTest)

include_directories ("${CMAKE_CURRENT_SOURCE_DIR}/../include")

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

set (CLANG_7_OR_LATER ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang" AND NOT (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7))

set (TESTCPP_CONTEXTUAL_BASIC_PROPERTIES "-Wno-gnu-zero-variadic-macro-arguments")
if (${CLANG_7_OR_LATER})
	string (APPEND TESTCPP_CONTEXTUAL_BASIC_PROPERTIES " -Wno-self-assign-overloaded")
endif (${CLANG_7_OR_LATER})
set_source_files_properties (testcpp_contextual_basic.cpp PROPERTIES COMPILE_FLAGS ${TESTCPP_CONTEXTUAL_BASIC_PROPERTIES})

# ~~~
# Disable warning about missing argument for `...` parameter of variadic macro.
# See also: https://github.com/google/googletest/issues/2271
# ~~~
set_source_files_properties (testkdb_allplugins.cpp testkdb_conflict.cpp PROPERTIES COMPILE_FLAGS "-Wno-gnu-zero-variadic-macro-arguments")
set (
	TESTS
	testcpp_contextual_basic.cpp
	testcpp_contextual_nocontext.cpp
	testcpp_contextual_policy.cpp
	testcpp_contextual_thread.cpp
	testcpp_contextual_update.cpp
	testcpp_iter_name.cpp
	testcpp_iter.cpp
	testcpp_kdb.cpp
	testcpp_key.cpp
	testcpp_keyio.cpp
	testcpp_ks.cpp
	testcpp_ksget.cpp
	testcpp_meta.cpp)
foreach (file ${TESTS})

	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_ELEKTRA elektra-kdb elektra-meta)
endforeach (file ${TESTS})

set_property (TEST testcpp_kdb PROPERTY LABELS kdbtests)
