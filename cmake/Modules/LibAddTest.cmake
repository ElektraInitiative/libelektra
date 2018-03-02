# Allows one to add test cases using Google Test
#

# recommended to use
#
# export GTEST_CATCH_EXCEPTIONS=0
#
# which disables a feature that seems to be popup-blocker for Windows

find_package(Threads)

macro (add_gtest source)
	cmake_parse_arguments (ARG
		"MEMLEAK;KDBTESTS;NO_MAIN;LINK_TOOLS" # optional keywords
		"" # one value keywords
		"LINK_LIBRARIES;LINK_ELEKTRA;SOURCES" # multi value keywords
		${ARGN}
	)

	if (BUILD_TESTING)
	set (SOURCES ${HDR_FILES} ${source}.cpp ${ARG_SOURCES})
	add_executable (${source} ${SOURCES})
	add_dependencies (${source} kdberrors_generated)

	if (ARG_LINK_TOOLS)
		target_link_elektratools(${source} ${ARG_LINK_ELEKTRA})
	else (ARG_LINK_TOOLS)
		target_link_elektra(${source} ${ARG_LINK_ELEKTRA})
	endif (ARG_LINK_TOOLS)

	target_link_libraries (${source}
		${ARG_LINK_LIBRARIES})

	if (NOT ARG_NO_MAIN)
		target_link_libraries (${source} gtest_main)
	endif (NOT ARG_NO_MAIN)

	target_link_libraries (${source} gtest)

	target_link_libraries(${source}
		${CMAKE_THREAD_LIBS_INIT})

	include_directories (SYSTEM ${GOOGLETEST_ROOT}/include)
	include_directories (${CMAKE_SOURCE_DIR}/tests/gtest-framework)

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

	if (ARG_MEMLEAK)
		set_property(TEST ${source} PROPERTY
			LABELS memleak)
	endif (ARG_MEMLEAK)
	if (ARG_KDBTESTS)
		set_property(TEST ${name} PROPERTY
			LABELS kdbtests)
	endif (ARG_KDBTESTS)
	endif(BUILD_TESTING)
endmacro (add_gtest)

# Add a Markdown Shell Recorder test for a certain Markdown file
#
# NAME: This argument specifies a postfix for the name of the CTest this function creates.
# FILE: This argument specifies the location of the Markdown file that contains the test data.
#
# REQUIRED_PLUGINS:
#	This optional variable specifies a list of plugins required to run the test.
#
# ENVIRONMENT:
# 	This optional argument specifies environment variables defined while CTest executes the MSR test.
function (add_s_test NAME FILE)
	set (TEST_NAME testshell_markdown_${NAME})
	set (oneValueArgs ENVIRONMENT)
	set (multiValueArgs REQUIRED_PLUGINS)
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	add_test (
		NAME testshell_markdown_${NAME}
		COMMAND "${CMAKE_BINARY_DIR}/tests/shell/shell_recorder/tutorial_wrapper/markdown_shell_recorder.sh" "${FILE}"
		WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
		)
	set_tests_properties (${TEST_NAME} PROPERTIES ENVIRONMENT "${ARG_ENVIRONMENT}")
	set_property(TEST ${TEST_NAME} PROPERTY LABELS memleak kdbtests)
endfunction ()

# Add a Markdown Shell Recorder test for a certain plugin
#
# PLUGIN: This argument specifies the name of the plugin for which this function creates a MSR test.
#
# ENVIRONMENT:
# 	This optional argument specifies environment variables defined while CTest executes the MSR test.
function (add_plugin_shell_test PLUGIN)
	add_s_test (${PLUGIN} "${CMAKE_SOURCE_DIR}/src/plugins/${PLUGIN}/README.md" ${ARGN} REQUIRED_PLUGINS ${PLUGIN})
endfunction ()
