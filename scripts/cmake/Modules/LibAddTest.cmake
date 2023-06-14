# ~~~
# Allows one to add test cases using Google Test
#
# recommended to use
#
# export GTEST_CATCH_EXCEPTIONS=0
#
# which disables a feature that seems to be popup-blocker for Windows
# ~~~

find_package (Threads QUIET)

# ~~~
# Add a Google Test for a list of specified source files.
#
# INCLUDE_DIRECTORIES:
# 	This optional variable specifies a list of include paths for the test.
# ~~~
macro (add_gtest source)
	cmake_parse_arguments (
		ARG
		"MEMLEAK;KDBTESTS;NO_MAIN;LINK_TOOLS" # optional keywords
		"" # one value keywords
		"INCLUDE_DIRECTORIES;LINK_LIBRARIES;LINK_ELEKTRA;SOURCES" # multi value keywords
		${ARGN})

	if (BUILD_TESTING)
		set (SOURCES ${HDR_FILES} ${source}.cpp ${ARG_SOURCES})
		add_executable (${source} ${SOURCES})

		if (ARG_LINK_TOOLS)
			target_link_elektratools (${source} ${ARG_LINK_ELEKTRA})
		else (ARG_LINK_TOOLS)
			target_link_elektra (${source} ${ARG_LINK_ELEKTRA})
		endif (ARG_LINK_TOOLS)

		target_link_libraries (${source} ${ARG_LINK_LIBRARIES})

		if (NOT ARG_NO_MAIN)
			target_link_libraries (${source} gtest_main)
		endif (NOT ARG_NO_MAIN)

		target_link_libraries (${source} gtest)

		target_link_libraries (${source} ${CMAKE_THREAD_LIBS_INIT})

		target_include_directories (${source} SYSTEM PRIVATE "${GOOGLETEST_ROOT}/include")
		target_include_directories (
			${source} PRIVATE "${CMAKE_SOURCE_DIR}/tests/gtest-framework" "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include"
					  "${CMAKE_SOURCE_DIR}/src/libs/tools/include")

		if (INSTALL_TESTING)
			install (
				TARGETS ${source}
				DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
				COMPONENT elektra-tests)
		endif (INSTALL_TESTING)

		set_target_properties (${source} PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
		set_property (
			TARGET ${source}
			APPEND
			PROPERTY INCLUDE_DIRECTORIES ${ARG_INCLUDE_DIRECTORIES})

		add_test (${source} "${CMAKE_BINARY_DIR}/bin/${source}" "${CMAKE_CURRENT_BINARY_DIR}/")
		set_property (TEST ${source} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib")

		if (ARG_MEMLEAK)
			set_property (TEST ${source} PROPERTY LABELS memleak)
		endif (ARG_MEMLEAK)
		if (ARG_KDBTESTS)
			set_property (TEST ${name} PROPERTY LABELS kdbtests)
			set_property (TEST ${name} PROPERTY RUN_SERIAL TRUE)
		endif (ARG_KDBTESTS)
	endif (BUILD_TESTING)
endmacro (add_gtest)

# ~~~
# Add a Markdown Shell Recorder test for a certain Markdown file
#
# NAME: This argument specifies a postfix for the name of the CTest this function creates.
# FILE: This argument specifies the location of the Markdown file that contains the test data.
#
# REQUIRED_PLUGINS:
# 	This optional variable specifies a list of plugins required to run the test.
#
# REQUIRED_TOOLS:
# 	This optional variable specifies a list of tools required to run the test.
#
# ENVIRONMENT:
# 	This optional argument specifies environment variables defined while CTest executes the MSR test.
# ~~~
function (add_msr_test NAME FILE)
	set (TEST_NAME testshell_markdown_${NAME})
	set (multiValueArgs ENVIRONMENT REQUIRED_PLUGINS REQUIRED_TOOLS)
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

	foreach (plugin ${ARG_REQUIRED_PLUGINS})
		list (FIND REMOVED_PLUGINS ${plugin} plugin_index)
		if (plugin_index GREATER -1)
			return ()
		endif (plugin_index GREATER -1)
	endforeach (plugin ${ARG_REQUIRED_PLUGINS})

	foreach (tool ${ARG_REQUIRED_TOOLS})
		list (FIND REMOVED_TOOLS ${tool} tool_index)
		if (tool_index GREATER -1)
			return ()
		endif ()

		list (FIND TOOLS ${tool} tool_index)
		if (tool_index LESS 0)
			return ()
		endif ()
	endforeach ()

	add_test (
		NAME testshell_markdown_${NAME}
		COMMAND "${CMAKE_BINARY_DIR}/tests/shell/shell_recorder/tutorial_wrapper/markdown_shell_recorder.sh" "${FILE}"
		WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
	set_property (TEST ${TEST_NAME} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib" "${ARG_ENVIRONMENT}")
	set_property (TEST ${TEST_NAME} PROPERTY LABELS memleak kdbtests)
	set_property (TEST ${TEST_NAME} PROPERTY RUN_SERIAL TRUE)
endfunction ()

# ~~~
# Add a Markdown Shell Recorder test for a certain plugin
#
# PLUGIN: This argument specifies the name of the plugin for which this function creates a MSR test.
#
# REQUIRED_PLUGINS:
# 	This optional variable specifies a list of plugins required to run the MSR test.
#
# REQUIRED_TOOLS:
# 	This optional variable specifies a list of tools required to run the test.
#
# ENVIRONMENT:
# 	This optional argument specifies environment variables defined while CTest executes the MSR test.
# ~~~
function (add_msr_test_plugin PLUGIN)
	set (multiValueArgs REQUIRED_PLUGINS REQUIRED_TOOLS)
	cmake_parse_arguments (ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
	list (APPEND ARG_REQUIRED_PLUGINS ${PLUGIN})
	list (APPEND ARG_REQUIRED_TOOLS ${TOOL})

	add_msr_test (
		${PLUGIN} "${CMAKE_SOURCE_DIR}/src/plugins/${PLUGIN}/README.md" ${ARGN}
		REQUIRED_PLUGINS ${ARG_REQUIRED_PLUGINS}
		REQUIRED_TOOLS ${ARG_REQUIRED_TOOLS})
endfunction ()
