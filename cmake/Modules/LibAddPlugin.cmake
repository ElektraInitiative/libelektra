include(LibAddMacros)

# Add a test for a plugin
#
# Will include the common tests.h file + its source file
# additional source files can be added as additional arguments.
# They need to be absolute or relative to the plugin dir.
#
# The testname must be the pluginname.
# By convention the source file needs to be called testmod_${pluginname}.c
#
# Prefer to use ADD_TEST of add_plugin which takes care
# of properly set all arguments so that the test is built
# in the same way as the plugin.
#
# LINK_PLUGIN:
#  Link against a different plugin (not the test name itself).
#  If you want to avoid linking against a plugin, use the string <no>.
#
# links the executable (only if build_static or build_full)
# and adds a test
function (add_plugintest testname)
	if (NOT ADDTESTING_PHASE)
		return ()
	endif ()

	if (BUILD_TESTING)
		cmake_parse_arguments (ARG
			"MEMLEAK;INSTALL_TEST_DATA" # optional keywords
			""        # one value keywords
			"COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES;LINK_LIBRARIES;LINK_ELEKTRA;LINK_PLUGIN;WORKING_DIRECTORY" # multi value keywords
			${ARGN}
		)

		list (FIND ADDED_PLUGINS "${testname}" FOUND_NAME)
		if (FOUND_NAME EQUAL -1)
			if (NOT ARG_LINK_PLUGIN)
				message (FATAL_ERROR "Trying to add plugintest ${testname} but no such plugin was added (try to use LINK_PLUGIN)")
			endif ()
		endif ()


		set (PLUGIN_NAME elektra-${testname})
		restore_variable (${PLUGIN_NAME} ARG_LINK_LIBRARIES)
		restore_variable (${PLUGIN_NAME} ARG_COMPILE_DEFINITIONS)
		restore_variable (${PLUGIN_NAME} ARG_INCLUDE_DIRECTORIES)
		restore_variable (${PLUGIN_NAME} ARG_LINK_ELEKTRA)
		restore_variable (${PLUGIN_NAME} ARG_ADD_TEST)
		restore_variable (${PLUGIN_NAME} ARG_INSTALL_TEST_DATA)

		set (TEST_SOURCES
				$<TARGET_OBJECTS:cframework>
				)

		foreach (A ${ARG_UNPARSED_ARGUMENTS})
			if (EXISTS "${A}")
				list (APPEND TEST_SOURCES "${A}")
			else ()
				list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/${A}")
			endif ()
		endforeach ()

		add_headers(TEST_SOURCES)
		add_testheaders(TEST_SOURCES)
		include_directories ("${CMAKE_SOURCE_DIR}/tests/cframework")
		list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testmod_${testname}.c")
		if (BUILD_SHARED)
			set (PLUGIN_TARGET_OBJS "")
			if (ARG_LINK_PLUGIN)
				if (NOT ARG_LINK_PLUGIN STREQUAL "<no>")
					set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:elektra-${ARG_LINK_PLUGIN}-objects>")
				endif ()
			else ()
				#assume that testcase+plugin to be tested have same name:
				set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:elektra-${testname}-objects>")
			endif ()
			list (APPEND TEST_SOURCES "${PLUGIN_TARGET_OBJS}")
		endif ()
		set (testexename testmod_${testname})
		add_executable (${testexename} ${TEST_SOURCES})
		add_dependencies (${testexename} kdberrors_generated)

		# alternative approach to restore_variable
		#get_target_property(TARGET_COMPILE_DEFINITIONS PLUGIN_TARGET_OBJS COMPILE_DEFINITIONS)

		if (INSTALL_TESTING)
			install (TARGETS ${testexename}
				DESTINATION "${TARGET_TOOL_EXEC_FOLDER}")
			if (ARG_INSTALL_TEST_DATA)
				install (DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${testname}" DESTINATION "${TARGET_TEST_DATA_FOLDER}")
			endif ()
		endif (INSTALL_TESTING)

		target_link_elektra(${testexename} elektra-kdb elektra-plugin ${ARG_LINK_ELEKTRA})

		target_link_libraries (${testexename} ${ARG_LINK_LIBRARIES})
		set_target_properties (${testexename} PROPERTIES
				COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;ELEKTRA_PLUGIN_TEST;${ARG_COMPILE_DEFINITIONS}")
		set_property(TARGET ${testexename}
				APPEND PROPERTY INCLUDE_DIRECTORIES
				${ARG_INCLUDE_DIRECTORIES})

		if (ARG_WORKING_DIRECTORY)
			set (WORKING_DIRECTORY "${ARG_WORKING_DIRECTORY}")
		else ()
			set (WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
		endif ()

		add_test (NAME ${testexename}
				COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
				WORKING_DIRECTORY "${WORKING_DIRECTORY}"
				)
		if (ARG_MEMLEAK)
			set_property(TEST ${testexename} PROPERTY
				LABELS memleak)
		endif (ARG_MEMLEAK)
	endif ()
endfunction (add_plugintest)


function (plugin_check_if_included PLUGIN_SHORT_NAME)
	list (FIND PLUGINS "-${PLUGIN_SHORT_NAME}" FOUND_EXCLUDE_NAME)
	if (FOUND_EXCLUDE_NAME GREATER -1)
		set (NOT_INCLUDED "explicitely excluded" PARENT_SCOPE)
		# let explicit exclusion win
		return ()
	endif ()

	list (FIND PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		set (NOT_INCLUDED "silent" PARENT_SCOPE)
		# maybe it is included by category
	else ()
		# plugin is given explicit
		return ()
	endif ()

	FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/README.md contents)
	STRING (REGEX MATCH "- +infos/status *= *([-a-zA-Z0-9 ]*)" CATEGORIES "${contents}")
	STRING (REGEX REPLACE "- +infos/status *= *([-a-zA-Z0-9 ]*)" "\\1" CATEGORIES "${CATEGORIES}")
	STRING (REGEX REPLACE " " ";" CATEGORIES "${CATEGORIES}")

	STRING (REGEX MATCH "- +infos/provides *= *([a-zA-Z0-9 ]*)" PROVIDES "${contents}")
	STRING (REGEX REPLACE "- +infos/provides *= *([a-zA-Z0-9 ]*)" "\\1" PROVIDES "${PROVIDES}")
	STRING (REGEX REPLACE " " ";" PROVIDES "${PROVIDES}")
	list (APPEND CATEGORIES "ALL" "${PROVIDES}")
	STRING (TOUPPER "${CATEGORIES}" CATEGORIES)
	#message (STATUS "CATEGORIES ${CATEGORIES}")

	foreach (CAT ${CATEGORIES})
		list (FIND PLUGINS "-${CAT}" FOUND_EXCLUDE_CATEGORY)
		if (FOUND_EXCLUDE_CATEGORY GREATER -1)
			set (NOT_INCLUDED "excluded by category ${CAT}" PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()

	foreach (CAT ${CATEGORIES})
		list (FIND PLUGINS "${CAT}" FOUND_CATEGORY)
		if (FOUND_CATEGORY GREATER -1)
			set (NOT_INCLUDED "" PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()
endfunction ()

function (restore_variable PLUGIN_NAME VARIABLE)
	set (PROP_NAME "${PLUGIN_NAME}_${VARIABLE}")
	get_property (VAR GLOBAL PROPERTY "${PROP_NAME}")
	if (VAR)
		if (DEFINED ${VARIABLE})
			# both stored and given by user: do consistency check
			#message (STATUS "consistency check, plugin ${PLUGIN_NAME} got ${VARIABLE} reset to ${VAR}")
			if (NOT ${VARIABLE} STREQUAL "${VAR}")
				message (FATAL_ERROR "Internally inconsistency, plugin ${PLUGIN_NAME} got different values for variable ${VARIABLE}: '${${VARIABLE}}' != '${VAR}'")
			endif ()
		else ()
			# stored, but not given by user, use what was stored
			set (${VARIABLE} ${VAR} PARENT_SCOPE)
		endif ()
	else ()
		if (DEFINED ${VARIABLE})
			# given by user, but not stored: store it
			set_property (GLOBAL PROPERTY "${PROP_NAME}" "${${VARIABLE}}")
		endif ()
	endif ()
endfunction ()


# add_plugin: register and potentially add a plugin
#
# SOURCES:
#  The sources of the plugin
#
# LINK_LIBRARIES:
#  add here only add libraries found by cmake
#  do not add dependencies to Elektra, use LINK_ELEKTRA for that
#
# LINK_ELEKTRA:
#  allows you to selectively link against different Elektra libraries, the default is elektra-plugin
#
# COMPILE_DEFINITIONS:
#  Set additional macros for per-variant compilation.
#
# INCLUDE_DIRECTORIES:
#  Append to include path (globally+plugin specific).
#
# ADD_TEST:
#  Add a plugin test case written in C (alternatively you can use add_gtest)
#
# INSTALL_TEST_DATA:
#  Install a directory with test data which has the same name as the plugin.
#
function (add_plugin PLUGIN_SHORT_NAME)
	cmake_parse_arguments (ARG
		"CPP;ADD_TEST;INSTALL_TEST_DATA" # optional keywords
		"" # one value keywords
		"SOURCES;LINK_LIBRARIES;COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES;LINK_ELEKTRA" # multi value keywords
		${ARGN}
	)

	set (PLUGIN_NAME elektra-${PLUGIN_SHORT_NAME})
	set (PLUGIN_OBJS ${PLUGIN_NAME}-objects)
	set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:${PLUGIN_OBJS}>")

	restore_variable (${PLUGIN_NAME} ARG_LINK_LIBRARIES)
	restore_variable (${PLUGIN_NAME} ARG_SOURCES)
	restore_variable (${PLUGIN_NAME} ARG_COMPILE_DEFINITIONS)
	restore_variable (${PLUGIN_NAME} ARG_INCLUDE_DIRECTORIES)
	restore_variable (${PLUGIN_NAME} ARG_LINK_ELEKTRA)
	restore_variable (${PLUGIN_NAME} ARG_ADD_TEST)
	restore_variable (${PLUGIN_NAME} ARG_INSTALL_TEST_DATA)

	if (ARG_UNPARSED_ARGUMENTS)
		message (FATAL_ERROR "Parsed a wrong argument to plugin ${PLUGIN_SHORT_NAME}: ${ARG_UNPARSED_ARGUMENTS}")
	endif ()

	if (ADDTESTING_PHASE)
		if (ARG_INSTALL_TEST_DATA AND NOT ARG_ADD_TEST)
			if (INSTALL_TESTING)
				install (DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${PLUGIN_SHORT_NAME}" DESTINATION "${TARGET_TEST_DATA_FOLDER}")
			endif (INSTALL_TESTING)
		endif ()

		if (ARG_ADD_TEST)
			set (HAS_CPP "")
			if (ARG_CPP)
				set (HAS_CPP "CPP")
			endif (ARG_CPP)

			set (HAS_INSTALL_TEST_DATA "")
			if (ARG_INSTALL_TEST_DATA)
				set (HAS_INSTALL_TEST_DATA "INSTALL_TEST_DATA")
			endif (ARG_INSTALL_TEST_DATA)

			add_plugintest ("${PLUGIN_SHORT_NAME}"
					LINK_LIBRARIES "${ARG_LINK_LIBRARIES}"
					LINK_PLUGIN "${PLUGIN_SHORT_NAME}"
					COMPILE_DEFINITIONS "${ARG_COMPILE_DEFINITIONS}"
					INCLUDE_DIRECTORIES "${ARG_INCLUDE_DIRECTORIES}"
					LINK_ELEKTRA "${ARG_LINK_ELEKTRA}"
					"${HAS_CPP}"
					"${HAS_INSTALL_TEST_DATA}"
					)
		endif ()
		return ()
	endif ()


	#message (STATUS "enter add_plugin ${PLUGIN_SHORT_NAME}")

	if (COLLECTION_PHASE)
		set (NOT_INCLUDED "")
		plugin_check_if_included ("${PLUGIN_SHORT_NAME}")

		if (NOT_INCLUDED)
			remove_plugin ("${PLUGIN_SHORT_NAME}" "${NOT_INCLUDED}")
			return ()
		endif ()

		if (ADDED_PLUGINS)
			set (TMP "${ADDED_PLUGINS};${PLUGIN_SHORT_NAME}")
			list (SORT TMP)
			list (REMOVE_DUPLICATES TMP)
			set (ADDED_PLUGINS "${TMP}" CACHE STRING "${ADDED_PLUGINS_DOC}" FORCE)
		else ()
			set (ADDED_PLUGINS "${PLUGIN_SHORT_NAME}" CACHE STRING "${ADDED_PLUGINS_DOC}" FORCE)
		endif ()

		# needed for variants where PLUGIN_SHORT_NAME != PLUGIN_FOLDER_NAME
		get_filename_component(PLUGIN_FOLDER_NAME ${CMAKE_CURRENT_LIST_DIR} NAME)
		if (ADDED_DIRECTORIES)
			set (TMP "${ADDED_DIRECTORIES};${PLUGIN_FOLDER_NAME}")
			list (SORT TMP)
			list (REMOVE_DUPLICATES TMP)
			set (ADDED_DIRECTORIES "${TMP}" CACHE STRING "${ADDED_DIRECTORIES_DOC}" FORCE)
		else ()
			set (ADDED_DIRECTORIES "${PLUGIN_FOLDER_NAME}" CACHE STRING "${ADDED_DIRECTORIES_DOC}" FORCE)
		endif ()

		return()
	endif ()


	list (FIND ADDED_PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		list (FIND REMOVED_PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
		if (FOUND_NAME EQUAL -1)
			message (FATAL_ERROR "Internally inconsistency, plugin ${PLUGIN_SHORT_NAME} is not there, but was not removed: ${REMOVED_PLUGINS}")
		endif ()
		# plugin was not added in previous phase or removed because of missing deps, exit quietly
		return ()
	endif ()

	if (NOT DEPENDENCY_PHASE)
		message (FATAL_ERROR "Assert failed, should be in dependency phase")
	endif ()

	#message (STATUS "name: ${PLUGIN_NAME}")
	#message (STATUS "srcs are: ${ARG_SOURCES}")
	#message (STATUS "deps are: ${ARG_LINK_LIBRARIES}")
	#message (STATUS "comp are: ${ARG_COMPILE_DEFINITIONS}")
	#message (STATUS "incl are: ${ARG_INCLUDE_DIRECTORIES}")
	#message (STATUS "current bin ${CMAKE_CURRENT_BINARY_DIR}")

	message (STATUS "Include Plugin ${PLUGIN_SHORT_NAME}")

	add_headers(ARG_SOURCES)
	if (ARG_CPP)
		add_cppheaders(ARG_SOURCES)
	endif (ARG_CPP)

	add_library (${PLUGIN_OBJS} OBJECT ${ARG_SOURCES})
	add_dependencies (${PLUGIN_OBJS} kdberrors_generated)

	add_dependencies(${PLUGIN_OBJS} readme_${PLUGIN_SHORT_NAME}.c)

	generate_readme (${PLUGIN_SHORT_NAME})

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY COMPILE_DEFINITIONS
		${ARG_COMPILE_DEFINITIONS}
		"ELEKTRA_STATIC"
		"HAVE_KDBCONFIG_H"
		"PLUGIN_SHORT_NAME=${PLUGIN_SHORT_NAME}"
		"README=readme_${PLUGIN_SHORT_NAME}.c"
		)

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY INCLUDE_DIRECTORIES
		${ARG_INCLUDE_DIRECTORIES}
		${CMAKE_CURRENT_BINARY_DIR} #for readme
		)

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY COMPILE_FLAGS
		${CMAKE_PIC_FLAGS} # needed for shared libraries
		)

	# needs cmake 3.0:
	#set_property(TARGET ${PLUGIN_OBJS}
	#	PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)

	if (BUILD_SHARED)
		add_library (${PLUGIN_NAME} MODULE ${ARG_SOURCES})
		add_dependencies (${PLUGIN_NAME} kdberrors_generated)
		if (ARG_LINK_ELEKTRA)
			target_link_libraries (${PLUGIN_NAME} elektra-plugin ${ARG_LINK_ELEKTRA})
		else()
			target_link_libraries (${PLUGIN_NAME} elektra-plugin)
		endif ()
		target_link_libraries (${PLUGIN_NAME}
			${ARG_LINK_LIBRARIES})
		install (TARGETS ${PLUGIN_NAME} DESTINATION
			lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
		set_property(TARGET ${PLUGIN_NAME}
			APPEND PROPERTY
			COMPILE_DEFINITIONS
			${ARG_COMPILE_DEFINITIONS}
			"HAVE_KDBCONFIG_H"
			"PLUGIN_SHORT_NAME=${PLUGIN_SHORT_NAME}"
			"README=readme_${PLUGIN_SHORT_NAME}.c"
			)
		set_property(TARGET ${PLUGIN_NAME}
			APPEND PROPERTY INCLUDE_DIRECTORIES
			${ARG_INCLUDE_DIRECTORIES}
			${CMAKE_CURRENT_BINARY_DIR} #for readme
			)
		if (${LD_ACCEPTS_VERSION_SCRIPT})
			set_property(TARGET ${PLUGIN_NAME}
				APPEND PROPERTY LINK_FLAGS
				"-Wl,--version-script=${PROJECT_SOURCE_DIR}/src/plugins/plugin-symbols.map"
				)
		endif ()
	endif()

	#message (STATUS "added ${PLUGIN_TARGET_OBJS}")
	set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS"
		${PLUGIN_TARGET_OBJS}
		)

	set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES"
		"${ARG_LINK_LIBRARIES}"
		)
endfunction(add_plugin)
