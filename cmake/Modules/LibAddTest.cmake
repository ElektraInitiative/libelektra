# Allows to add test cases using Google Test
#
# If ENABLE_EXTERNAL_GTEST is set,
# this script reads the GTEST_ROOT variable, which must be passed,
# looking into it as base directory of the GoogleTest sources.
#
# Otherwise it will use the source as included with Elektra.

#against multiple inclusion
if(NOT GOOGLETEST_ROOT)
	if(NOT (GTEST_ROOT STREQUAL ""))
		if(EXISTS ${GTEST_ROOT}/CMakeLists.txt)
			set(GOOGLETEST_ROOT "${GTEST_ROOT}")
			message (STATUS "used external gtest below ${GTEST_ROOT}")
			set(GOOGLETEST_FOUND TRUE)
		endif()
	else()
		set(GOOGLETEST_ROOT ${CMAKE_SOURCE_DIR}/tests/gtest-1.7.0)
		set(GOOGLETEST_FOUND TRUE)
	endif()

	if (GOOGLETEST_FOUND)
		add_subdirectory(${GOOGLETEST_ROOT} ${CMAKE_CURRENT_BINARY_DIR}/gtest)
		include_directories(SYSTEM ${GOOGLETEST_ROOT}/include)

		set_property (TARGET gtest PROPERTY COMPILE_FLAGS "-w" )
		set_property (TARGET gtest_main PROPERTY COMPILE_FLAGS "-w" )
	else ()
		message(SEND_ERROR "googletest not found")
	endif ()
endif()


macro (add_gtest source)
	if (BUILD_TESTING)
	set (SOURCES ${HDR_FILES} ${source}.cpp)
	add_executable (${source} ${SOURCES})

	#do not hardcode tools here:
	target_link_elektratools(${source})

	target_link_libraries(${source} gtest gtest_main)

	if (INSTALL_TESTING)
		install (TARGETS ${source}
			DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
	endif (INSTALL_TESTING)

	set_target_properties (${source} PROPERTIES
			COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
	add_test (${source}
			"${CMAKE_BINARY_DIR}/bin/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}/"
			)
	endif(BUILD_TESTING)
endmacro (add_gtest)
