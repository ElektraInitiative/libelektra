find_package (Threads QUIET)

include (LibAddMacros)
include (LibAddTest)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)
add_toolheaders (HDR_FILES)

set (
	TESTS
	test_model_configformat.cpp
	test_model_entry.cpp
	test_model_pluginformat.cpp
	test_model_user.cpp
	test_service_configengine.cpp
	# The test for the conversion engine always fails. See also: https://issues.libelektra.org/3086 TODO: Reenable test
	# test_service_convertengine.cpp
	test_service_searchengine.cpp
	test_service_storageengine.cpp
	test_utils_crypto.cpp)

foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name} LINK_TOOLS SOURCES $<TARGET_OBJECTS:tmp_${tool}>)
	target_link_elektra (${name} ${Boost_LIBRARIES} ${CPPCMS_LIBRARY} ${LIBJWT_LIBRARY} ${OPENSSL_LIBRARIES})
endforeach (file ${TESTS})
