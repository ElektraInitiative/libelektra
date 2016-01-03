# Allows one to add test cases using Google Test
#

# recommended to use
#
# export GTEST_CATCH_EXCEPTIONS=0
#
# which disables a feature that seems to be popup-blocker for Windows


macro (add_gtest source)
	cmake_parse_arguments (ARG
		"MEMLEAK;NO_MAIN;NO_TOOLS" # optional keywords
		"" # one value keywords
		"LINK_LIBRARIES;SOURCES" # multi value keywords
		${ARGN}
	)

	if (BUILD_TESTING)
	set (SOURCES ${HDR_FILES} ${source}.cpp ${ARG_SOURCES})
	add_executable (${source} ${SOURCES})

	if (NOT ARG_NO_TOOLS)
		# invert logic?
		target_link_elektratools(${source})
	endif (NOT ARG_NO_TOOLS)

	target_link_libraries (${source}
		${ARG_LINK_LIBRARIES})

	target_link_libraries(${source} gtest)
	if (NOT ARG_NO_MAIN)
		target_link_libraries(${source} gtest_main)
	endif (NOT ARG_NO_MAIN)

	include_directories(SYSTEM ${GOOGLETEST_ROOT}/include)
	include_directories(${CMAKE_SOURCE_DIR}/tests/gtest-framework)

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

	if (ARG_MEMLEAK)
		set_property(TEST ${source} PROPERTY
			LABELS memleak)
	endif (ARG_MEMLEAK)
endmacro (add_gtest)
