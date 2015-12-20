include(LibAddTest)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_headers (HDR_FILES)
add_cppheaders (HDR_FILES)

#macro (do_cpp_test source)
#	include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
#	set (SOURCES ${HDR_FILES} ${source}.cpp tests.cpp tests.hpp)
#	add_executable (${source} ${SOURCES})
#
#	target_link_elektra(${source})
#
#	if (INSTALL_TESTING)
#		install (TARGETS ${source}
#			DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
#	endif (INSTALL_TESTING)
#
#	set_target_properties (${source} PROPERTIES
#			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
#	add_test (${source}
#			"${CMAKE_BINARY_DIR}/bin/${source}"
#			"${CMAKE_CURRENT_BINARY_DIR}/"
#			)
#endmacro (do_cpp_test)
#

file (GLOB TESTS testcpp_*.cpp)
foreach (file ${TESTS})
	get_filename_component (name ${file} NAME_WE)
	add_gtest (${name})
endforeach (file ${TESTS})

set_property(TEST testcpp_kdb
	PROPERTY LABELS kdbtests)
