include (LibAddMacros)
include (LibAddTest)

# ~~~
# Provides additional compiler definitions:
#
# - ELEKTRA_PLUGIN_NAME containing the short plugin name as string
# - ELEKTRA_PLUGIN_NAME_C containing the short plugin name which can be used in function names
#
# shortname:
#   pass the PLUGIN_SHORT_NAME as this argument
#
# ADDITIONAL_COMPILE_DEFINITIONS:
#   will contain the additional compiler definitions mentioned above.
#   will be set to the parent scope, i.e. the caller of the function
#
# ~~~
function (set_additional_compile_definitions shortname)
	# provide the plugin name as string to the compiler/preprocessor
	if (NOT "${ARG_COMPILE_DEFINITIONS}" MATCHES "ELEKTRA_PLUGIN_NAME")
		list (APPEND ADDITIONAL_COMPILE_DEFINITIONS_PARTS "ELEKTRA_PLUGIN_NAME=\"${shortname}\"")
	endif ()

	# provide the plugin name as macro that can be used for building function names, etc.
	if (NOT "${ARG_COMPILE_DEFINITIONS}" MATCHES "ELEKTRA_PLUGIN_NAME_C")
		list (APPEND ADDITIONAL_COMPILE_DEFINITIONS_PARTS "ELEKTRA_PLUGIN_NAME_C=${shortname}")
	endif ()

	if (NOT "${ARG_COMPILE_DEFINITIONS}" MATCHES "ELEKTRA_MODULE_NAME")
		list (APPEND ADDITIONAL_COMPILE_DEFINITIONS_PARTS "ELEKTRA_MODULE_NAME=${shortname}")
	endif ()

	set (
		ADDITIONAL_COMPILE_DEFINITIONS
		"${ADDITIONAL_COMPILE_DEFINITIONS_PARTS}"
		PARENT_SCOPE)
	unset (ADDITIONAL_COMPILE_DEFINITIONS_PARTS)
endfunction (set_additional_compile_definitions)

# ~~~
# Add a test for a plugin
#
# Will include the common tests.h file + its source file
# additional source files can be added as additional arguments.
# They need to be absolute or relative to the plugin dir.
#
# The testname must be the pluginname.
# By convention the source file needs to be called `testmod_${pluginname}.c`
# or `testmod_${pluginname}.cpp`, if you specify the optional keyword `CPP`.
#
# Prefer to use ADD_TEST of add_plugin which takes care
# of properly set all arguments so that the test is built
# in the same way as the plugin.
#
# CPP:
#  Build a C++ test instead of a C test.
#
# ENVIRONMENT:
#   Specifies environment variables set while the build system executes the test
#
# LINK_PLUGIN:
#  Link against a different plugin (not the test name itself).
#  If you want to avoid linking against a plugin, use the string <no>.
#
# links the executable (only if build_static or build_full)
# and adds a test
# ~~~
function (add_plugintest testname)
	if (NOT ADDTESTING_PHASE)
		return ()
	endif ()

	if (BUILD_TESTING)
		set (
			MULTI_VALUE_KEYWORDS
			COMPILE_DEFINITIONS
			INCLUDE_DIRECTORIES
			INCLUDE_SYSTEM_DIRECTORIES
			LINK_LIBRARIES
			LINK_ELEKTRA
			TEST_LINK_LIBRARIES
			TEST_LINK_ELEKTRA
			LINK_PLUGIN
			ENVIRONMENT
			TIMEOUT
			WORKING_DIRECTORY
			EXTRA_EXECUTABLES)

		cmake_parse_arguments (
			ARG
			"MEMLEAK;INSTALL_TEST_DATA;CPP;USE_LINK_RPATH;NO_INSTALL" # optional keywords
			"" # one value keywords
			"${MULTI_VALUE_KEYWORDS}" # multi value keywords
			${ARGN})

		if (ARG_LINK_PLUGIN)
			set (testplugin "${ARG_LINK_PLUGIN}")
		else (ARG_LINK_PLUGIN)
			set (testplugin "${testname}")
		endif (ARG_LINK_PLUGIN)

		list (FIND ADDED_PLUGINS "${testplugin}" FOUND_NAME)
		if (FOUND_NAME EQUAL -1)
			if (NOT ARG_LINK_PLUGIN)
				message (
					FATAL_ERROR
						"Trying to add plugintest ${testplugin} but no such plugin was added (try to use LINK_PLUGIN)"
				)
			endif ()

			# plugin for test was not added in previous phase or removed because of missing deps, exit quietly
			return ()
		endif ()

		set (PLUGIN_NAME elektra-${testplugin})
		restore_variable (${PLUGIN_NAME} ARG_LINK_LIBRARIES)
		restore_variable (${PLUGIN_NAME} ARG_COMPILE_DEFINITIONS)
		restore_variable (${PLUGIN_NAME} ARG_INCLUDE_DIRECTORIES)
		restore_variable (${PLUGIN_NAME} ARG_INCLUDE_SYSTEM_DIRECTORIES)
		restore_variable (${PLUGIN_NAME} ARG_LINK_ELEKTRA)
		restore_variable (${PLUGIN_NAME} ARG_ADD_TEST)
		restore_variable (${PLUGIN_NAME} ARG_INSTALL_TEST_DATA)
		restore_variable (${PLUGIN_NAME} ARG_CPP)
		restore_variable (${PLUGIN_NAME} ARG_OBJECT_SOURCES)
		restore_variable (${PLUGIN_NAME} ARG_TEST_LINK_LIBRARIES)
		restore_variable (${PLUGIN_NAME} ARG_TEST_LINK_ELEKTRA)
		restore_variable (${PLUGIN_NAME} ARG_ENVIRONMENT)
		restore_variable (${PLUGIN_NAME} ARG_TIMEOUT)
		restore_variable (${PLUGIN_NAME} ARG_EXTRA_EXECUTABLES)
		restore_variable (${PLUGIN_NAME} ARG_USE_LINK_RPATH)
		restore_variable (${PLUGIN_NAME} ARG_NO_INSTALL)

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework> ${ARG_OBJECT_SOURCES})

		foreach (A ${ARG_UNPARSED_ARGUMENTS})
			if (EXISTS "${A}")
				list (APPEND TEST_SOURCES "${A}")
			else ()
				list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/${A}")
			endif ()
		endforeach ()

		add_headers (TEST_SOURCES)
		add_testheaders (TEST_SOURCES)

		if (ARG_CPP)
			list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testmod_${testname}.cpp")
		else (ARG_CPP)
			list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testmod_${testname}.c")
		endif (ARG_CPP)

		set (PLUGIN_TARGET_OBJS "")
		if (ARG_LINK_PLUGIN)
			if (NOT ARG_LINK_PLUGIN STREQUAL "<no>")
				set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:elektra-${ARG_LINK_PLUGIN}-objects>")
			endif ()
		else ()
			set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:elektra-${testname}-objects>") # assume that test case+plugin to be tested
			# have same name:
		endif ()
		list (APPEND TEST_SOURCES "${PLUGIN_TARGET_OBJS}")

		set (testexename testmod_${testname})
		add_executable (${testexename} ${TEST_SOURCES})

		if (ARG_CPP)
			target_include_directories (
				${testexename}
				PRIVATE ${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests
				PRIVATE ${CMAKE_SOURCE_DIR}/src/bindings/cpp/include)
		endif (ARG_CPP)
		target_include_directories (${testexename} PRIVATE "${CMAKE_SOURCE_DIR}/tests/cframework")

		if (BUILD_SHARED)
			if (ARG_LINK_PLUGIN)
				add_dependencies (${testexename} elektra-${ARG_LINK_PLUGIN})
			else ()
				add_dependencies (${testexename} elektra-${testname})
			endif ()
		endif ()

		# ~~~
		# alternative approach to restore_variable
		# get_target_property(TARGET_COMPILE_DEFINITIONS PLUGIN_TARGET_OBJS COMPILE_DEFINITIONS)
		# ~~~

		if (INSTALL_TESTING AND NOT ARG_NO_INSTALL)
			install (
				TARGETS ${testexename}
				DESTINATION "${TARGET_TOOL_EXEC_FOLDER}"
				COMPONENT elektra-tests)
			foreach (ee ${ARG_EXTRA_EXECUTABLES})
				install (
					TARGETS ${ee}
					DESTINATION "${TARGET_TOOL_EXEC_FOLDER}"
					COMPONENT elektra-tests)
			endforeach (ee ${ARG_EXTRA_EXECUTABLES})

			if (ARG_INSTALL_TEST_DATA)
				install (
					DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${testname}"
					DESTINATION "${TARGET_TEST_DATA_FOLDER}"
					COMPONENT elektra-tests)
			endif ()
		endif (INSTALL_TESTING AND NOT ARG_NO_INSTALL)

		target_link_elektra (${testexename} elektra-kdb elektra-plugin ${ARG_LINK_ELEKTRA} ${ARG_TEST_LINK_ELEKTRA})

		if (ARG_CPP)
			target_link_libraries (${testexename} gtest_main)
		endif (ARG_CPP)

		set_additional_compile_definitions (${testname})

		target_link_libraries (${testexename} ${ARG_LINK_LIBRARIES} ${ARG_TEST_LINK_LIBRARIES})
		set_target_properties (
			${testexename}
			PROPERTIES COMPILE_DEFINITIONS
				   "HAVE_KDBCONFIG_H;ELEKTRA_PLUGIN_TEST;${ARG_COMPILE_DEFINITIONS};${ADDITIONAL_COMPILE_DEFINITIONS}")
		set_property (
			TARGET ${testexename}
			APPEND
			PROPERTY INCLUDE_DIRECTORIES ${ARG_INCLUDE_DIRECTORIES})
		# do not strip rpath during install
		if (ARG_USE_LINK_RPATH)
			set_target_properties (${testexename} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
		endif ()
		unset (ADDITIONAL_COMPILE_DEFINITIONS)

		foreach (DIR ${ARG_INCLUDE_SYSTEM_DIRECTORIES})

			if (DIR AND NOT DIR STREQUAL "/usr/include")
				set_property (
					TARGET ${testexename}
					APPEND_STRING
					PROPERTY COMPILE_FLAGS " ${CMAKE_INCLUDE_SYSTEM_FLAG_CXX} \"${DIR}\"")
			endif ()
		endforeach (DIR)

		if (ARG_WORKING_DIRECTORY)
			set (WORKING_DIRECTORY "${ARG_WORKING_DIRECTORY}")
		else ()
			set (WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
		endif ()

		add_test (
			NAME ${testexename}
			COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
			WORKING_DIRECTORY "${WORKING_DIRECTORY}")

		if (ARG_TIMEOUT)
			set_tests_properties (${testexename} PROPERTIES TIMEOUT "${ARG_TIMEOUT}")
		endif (ARG_TIMEOUT)

		set_property (TEST ${testexename} PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib"
							   "KDB_TEST_BIN_DIR=${CMAKE_BINARY_DIR}/bin" "${ARG_ENVIRONMENT}")

		if (ARG_MEMLEAK)
			set_property (TEST ${testexename} PROPERTY LABELS memleak)
		endif (ARG_MEMLEAK)
	endif ()
endfunction (add_plugintest)

function (plugin_check_if_included PLUGIN_SHORT_NAME)
	set (
		NOT_INCLUDED
		""
		PARENT_SCOPE)
	list (FIND PLUGINS "-${PLUGIN_SHORT_NAME}" FOUND_EXCLUDE_NAME)
	if (FOUND_EXCLUDE_NAME GREATER -1)
		set (
			NOT_INCLUDED
			"explicitly excluded"
			PARENT_SCOPE) # let explicit exclusion win

		return ()
	endif ()

	list (FIND PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		set (
			NOT_INCLUDED
			"silent"
			PARENT_SCOPE) # maybe it is included by category

	else ()

		return () # plugin is given explicit
	endif ()

	file (READ ${CMAKE_CURRENT_SOURCE_DIR}/README.md contents)
	string (REGEX MATCH "- +infos/status *= *([-a-zA-Z0-9 ]*)" CATEGORIES "${contents}")
	string (REGEX REPLACE "- +infos/status *= *([-a-zA-Z0-9 ]*)" "\\1" CATEGORIES "${CATEGORIES}")
	string (REGEX REPLACE " " ";" CATEGORIES "${CATEGORIES}")

	string (REGEX MATCH "- +infos/provides *= *([a-zA-Z0-9/ ]*)" PROVIDES "${contents}")
	string (REGEX REPLACE "- +infos/provides *= *([a-zA-Z0-9/ ]*)" "\\1" PROVIDES "${PROVIDES}")
	string (REGEX REPLACE " " ";" PROVIDES "${PROVIDES}")
	list (APPEND CATEGORIES "ALL" "${PROVIDES}")

	split_plugin_providers (CATEGORIES)

	string (TOUPPER "${CATEGORIES}" CATEGORIES) # message (STATUS "CATEGORIES ${CATEGORIES}")

	foreach (CAT ${CATEGORIES})
		list (FIND PLUGINS "-${CAT}" FOUND_EXCLUDE_CATEGORY)
		if (FOUND_EXCLUDE_CATEGORY GREATER -1)
			set (
				NOT_INCLUDED
				"excluded by category ${CAT}"
				PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()

	foreach (CAT ${CATEGORIES})
		list (FIND PLUGINS "${CAT}" FOUND_CATEGORY)
		if (FOUND_CATEGORY GREATER -1)
			set (
				NOT_INCLUDED
				""
				PARENT_SCOPE)
			return ()
		endif ()
	endforeach ()
endfunction ()

function (restore_variable PLUGIN_NAME VARIABLE)
	set (PROP_NAME "${PLUGIN_NAME}_${VARIABLE}")
	get_property (VAR GLOBAL PROPERTY "${PROP_NAME}")
	if (VAR)
		if (DEFINED ${VARIABLE})
			# ~~~
			# both stored and given by user: do consistency check
			# message (STATUS "consistency check, plugin ${PLUGIN_NAME} got ${VARIABLE} reset to ${VAR}")
			# ~~~
			if (NOT ${VARIABLE} STREQUAL "${VAR}")
				message (
					FATAL_ERROR
						"Internally inconsistency, plugin ${PLUGIN_NAME} got different values for variable ${VARIABLE}: '${${VARIABLE}}' != '${VAR}'. "
						"Concatenate variables that contain the value of ${VARIABLE} in a single variable during the DEPENDENCY_PHASE. "
						"Make sure that only a single variable is passed to every argument of add_plugin.")
			endif ()
		else ()

			# stored, but not given by user, use what was stored
			set (
				${VARIABLE}
				${VAR}
				PARENT_SCOPE)
		endif ()
	else ()
		if (DEFINED ${VARIABLE})

			# given by user, but not stored: store it
			set_property (GLOBAL PROPERTY "${PROP_NAME}" "${${VARIABLE}}")
		endif ()
	endif ()
endfunction ()

# ~~~
# add_plugin: register and potentially add a plugin
#
# SOURCES:
#  The sources of the plugin
#
# OBJECT_SOURCES:
#  Object library sources for the plugin
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
# INCLUDE_SYSTEM_DIRECTORIES:
#  Same as INCLUDE_DIRECTORIES, but avoids warnings and dependency calculation
#  for these include folders.
#
# ONLY_SHARED:
#  Do not include this plugin in FULL or STATIC builds.
#
# ADD_TEST:
#  Add a plugin test case written in C (alternatively you can use add_gtest)
#
# CPP_TEST:
#  If you add this optional keyword, then the function will add
#  a C++ test instead of a C test. This argument only makes sense if you also
#  specified the keyword `ADD_TEST`.
#
# INSTALL_TEST_DATA:
#  Install a directory with test data which has the same name as the plugin.
#
# TEST_README:
#  Add a Markdown Shell Recorder test for the ReadMe of the plugin
#
# TEST_ENVIRONMENT:
#   Specifies environment variables set while the build system executes the plugin tests
#
# TEST_REQUIRED_PLUGINS:
#   Specifies a list of required plugins for the **Markdown Shell Recorder** test
#
# TEST_LINK_LIBRARIES:
#   like LINK_LIBRARIES but only applies to plugin tests
#
# TEST_LINK_ELEKTRA:
#   like LINK_ELEKTRA but only applies to plugin tests
# ~~~
function (add_plugin PLUGIN_SHORT_NAME)
	set (
		MULTI_VALUE_KEYWORDS
		SOURCES
		OBJECT_SOURCES
		LINK_LIBRARIES
		COMPILE_DEFINITIONS
		INCLUDE_DIRECTORIES
		INCLUDE_SYSTEM_DIRECTORIES
		LINK_ELEKTRA
		DEPENDS
		TEST_ENVIRONMENT
		TEST_REQUIRED_PLUGINS)
	cmake_parse_arguments (
		ARG
		"CPP;CPP_TEST;ADD_TEST;TEST_README;INSTALL_TEST_DATA;ONLY_SHARED;USE_LINK_RPATH" # optional keywords
		"COMPONENT" # one value keywords
		"${MULTI_VALUE_KEYWORDS}" # multi value keywords
		${ARGN})

	set (PLUGIN_NAME elektra-${PLUGIN_SHORT_NAME})
	set (PLUGIN_OBJS ${PLUGIN_NAME}-objects)
	set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:${PLUGIN_OBJS}>")

	restore_variable (${PLUGIN_NAME} ARG_LINK_LIBRARIES)
	restore_variable (${PLUGIN_NAME} ARG_SOURCES)
	restore_variable (${PLUGIN_NAME} ARG_OBJECT_SOURCES)
	restore_variable (${PLUGIN_NAME} ARG_COMPILE_DEFINITIONS)
	restore_variable (${PLUGIN_NAME} ARG_INCLUDE_DIRECTORIES)
	restore_variable (${PLUGIN_NAME} ARG_INCLUDE_SYSTEM_DIRECTORIES)
	restore_variable (${PLUGIN_NAME} ARG_LINK_ELEKTRA)
	restore_variable (${PLUGIN_NAME} ARG_ADD_TEST)
	restore_variable (${PLUGIN_NAME} ARG_CPP_TEST)
	restore_variable (${PLUGIN_NAME} ARG_TEST_README)
	restore_variable (${PLUGIN_NAME} ARG_TEST_ENVIRONMENT)
	restore_variable (${PLUGIN_NAME} ARG_TEST_REQUIRED_PLUGINS)
	restore_variable (${PLUGIN_NAME} ARG_INSTALL_TEST_DATA)
	restore_variable (${PLUGIN_NAME} ARG_ONLY_SHARED)
	restore_variable (${PLUGIN_NAME} ARG_USE_LINK_RPATH)
	restore_variable (${PLUGIN_NAME} ARG_COMPONENT)

	if (ARG_UNPARSED_ARGUMENTS)
		message (FATAL_ERROR "Parsed a wrong argument to plugin ${PLUGIN_SHORT_NAME}: ${ARG_UNPARSED_ARGUMENTS}")
	endif ()

	if (ARG_COMPONENT)
		set (HAS_COMPONENT ${ARG_COMPONENT})
	else ()
		set (HAS_COMPONENT "${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME}")
	endif ()

	if (ADDTESTING_PHASE)
		if (ARG_INSTALL_TEST_DATA AND NOT ARG_ADD_TEST)
			if (INSTALL_TESTING)
				install (
					DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${PLUGIN_SHORT_NAME}"
					DESTINATION "${TARGET_TEST_DATA_FOLDER}"
					COMPONENT elektra-tests)
			endif (INSTALL_TESTING)
		endif ()

		if (ARG_ADD_TEST)
			set (HAS_INSTALL_TEST_DATA "")
			if (ARG_INSTALL_TEST_DATA)
				set (HAS_INSTALL_TEST_DATA "INSTALL_TEST_DATA")
			endif (ARG_INSTALL_TEST_DATA)

			set (CPP_TEST "")
			if (ARG_CPP_TEST)
				set (CPP_TEST "CPP")
			endif (ARG_CPP_TEST)

			add_plugintest (
				"${PLUGIN_SHORT_NAME}"
				LINK_LIBRARIES "${ARG_LINK_LIBRARIES}"
				LINK_PLUGIN "${PLUGIN_SHORT_NAME}"
				COMPILE_DEFINITIONS "${ARG_COMPILE_DEFINITIONS}"
				INCLUDE_DIRECTORIES "${ARG_INCLUDE_DIRECTORIES}"
				INCLUDE_SYSTEM_DIRECTORIES "${ARG_INCLUDE_SYSTEM_DIRECTORIES}"
				ENVIRONMENT "${ARG_TEST_ENVIRONMENT}"
				LINK_ELEKTRA "${ARG_LINK_ELEKTRA}" "${HAS_INSTALL_TEST_DATA}" ${CPP_TEST})
		endif ()

		if (ARG_TEST_README AND ENABLE_KDB_TESTING)
			add_msr_test_plugin (
				${PLUGIN_SHORT_NAME}
				ENVIRONMENT ${ARG_TEST_ENVIRONMENT}
				REQUIRED_PLUGINS ${ARG_TEST_REQUIRED_PLUGINS})
		endif (ARG_TEST_README AND ENABLE_KDB_TESTING)

		return ()
	endif ()

	# message (STATUS "enter add_plugin ${PLUGIN_SHORT_NAME}")

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
			set (
				ADDED_PLUGINS
				"${TMP}"
				CACHE STRING "${ADDED_PLUGINS_DOC}" FORCE)
		else ()
			set (
				ADDED_PLUGINS
				"${PLUGIN_SHORT_NAME}"
				CACHE STRING "${ADDED_PLUGINS_DOC}" FORCE)
		endif ()

		# needed for variants where PLUGIN_SHORT_NAME != PLUGIN_FOLDER_NAME
		get_filename_component (PLUGIN_FOLDER_NAME ${CMAKE_CURRENT_LIST_DIR} NAME)
		if (ADDED_DIRECTORIES)
			set (TMP "${ADDED_DIRECTORIES};${PLUGIN_FOLDER_NAME}")
			list (SORT TMP)
			list (REMOVE_DUPLICATES TMP)
			set (
				ADDED_DIRECTORIES
				"${TMP}"
				CACHE STRING "${ADDED_DIRECTORIES_DOC}" FORCE)
		else ()
			set (
				ADDED_DIRECTORIES
				"${PLUGIN_FOLDER_NAME}"
				CACHE STRING "${ADDED_DIRECTORIES_DOC}" FORCE)
		endif ()

		return ()
	endif ()

	list (FIND ADDED_PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
	if (FOUND_NAME EQUAL -1)
		list (FIND REMOVED_PLUGINS "${PLUGIN_SHORT_NAME}" FOUND_NAME)
		if (FOUND_NAME EQUAL -1)
			message (
				FATAL_ERROR
					"Internally inconsistency, plugin ${PLUGIN_SHORT_NAME} is not there, but was not removed: ${REMOVED_PLUGINS}"
			)
		endif () # plugin was not added in previous phase or removed because of missing deps, exit quietly
		return ()
	endif ()

	if (NOT DEPENDENCY_PHASE)
		message (FATAL_ERROR "Assert failed, should be in dependency phase")
	endif ()

	# ~~~
	# message (STATUS "name: ${PLUGIN_NAME}")
	# message (STATUS "srcs are: ${ARG_SOURCES}")
	# message (STATUS "deps are: ${ARG_LINK_LIBRARIES}")
	# message (STATUS "comp are: ${ARG_COMPILE_DEFINITIONS}")
	# message (STATUS "incl are: ${ARG_INCLUDE_DIRECTORIES}")
	# message (STATUS "system incl are: ${ARG_INCLUDE_SYSTEM_DIRECTORIES}")
	# message (STATUS "current bin ${CMAKE_CURRENT_BINARY_DIR}")
	# ~~~

	if (ARG_ONLY_SHARED AND NOT BUILD_SHARED)
		remove_plugin ("${PLUGIN_SHORT_NAME}"
			       "it can only be built if BUILD_SHARED is enabled, both BUILD_FULL or BUILD_STATIC may be enabled as well")
		return ()
	endif (ARG_ONLY_SHARED AND NOT BUILD_SHARED)

	set (STATUS_MESSAGE "Include plugin ${PLUGIN_SHORT_NAME}")
	if (ARG_ONLY_SHARED)

		# also add it to the list of ONLY_SHARED plugins for exportsymbols.c configuration
		set (STATUS_MESSAGE "${STATUS_MESSAGE} for shared builds")
		set_property (GLOBAL APPEND PROPERTY SHARED_ONLY_PLUGINS "${PLUGIN_SHORT_NAME}")
	endif (ARG_ONLY_SHARED)
	message (STATUS "${STATUS_MESSAGE}")

	add_headers (ARG_SOURCES)
	if (ARG_CPP)
		add_cppheaders (ARG_SOURCES)
	endif (ARG_CPP)

	add_library (${PLUGIN_OBJS} OBJECT ${ARG_SOURCES})

	if (ARG_DEPENDS)
		add_dependencies (${PLUGIN_OBJS} ${ARG_DEPENDS})
	endif ()

	generate_readme (${PLUGIN_SHORT_NAME})
	set_additional_compile_definitions (${PLUGIN_SHORT_NAME})

	set_property (
		TARGET ${PLUGIN_OBJS}
		APPEND
		PROPERTY COMPILE_DEFINITIONS
			 ${ARG_COMPILE_DEFINITIONS}
			 ${ADDITIONAL_COMPILE_DEFINITIONS}
			 "ELEKTRA_STATIC"
			 "HAVE_KDBCONFIG_H"
			 "PLUGIN_SHORT_NAME=${PLUGIN_SHORT_NAME}"
			 "README=readme_${PLUGIN_SHORT_NAME}.c")

	if (ARG_CPP)
		target_include_directories (${PLUGIN_OBJS} PUBLIC "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")
	endif (ARG_CPP)
	target_include_directories (${PLUGIN_OBJS} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}") # for readme
	set_property (
		TARGET ${PLUGIN_OBJS}
		APPEND
		PROPERTY INCLUDE_DIRECTORIES ${ARG_INCLUDE_DIRECTORIES})

	set_property (
		TARGET ${PLUGIN_OBJS}
		APPEND_STRING
		PROPERTY COMPILE_FLAGS "${CMAKE_PIC_FLAGS}" # needed for shared libraries
	)

	foreach (DIR ${ARG_INCLUDE_SYSTEM_DIRECTORIES})
		if (DIR AND NOT DIR STREQUAL "/usr/include")
			set_property (
				TARGET ${PLUGIN_OBJS}
				APPEND_STRING
				PROPERTY COMPILE_FLAGS " ${CMAKE_INCLUDE_SYSTEM_FLAG_CXX} \"${DIR}\"")
		endif (DIR AND NOT DIR STREQUAL "/usr/include")
	endforeach (DIR)

	set_property (TARGET ${PLUGIN_OBJS} PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)

	if (BUILD_SHARED)
		# TODO: why is this not built upon ${PLUGIN_OBJS}
		add_library (${PLUGIN_NAME} MODULE ${ARG_SOURCES} ${ARG_OBJECT_SOURCES})
		# TODO: switch to -plugin- in CMake targets
		set_target_properties (${PLUGIN_NAME} PROPERTIES OUTPUT_NAME "elektra-plugin-${PLUGIN_SHORT_NAME}")

		if (ARG_CPP)
			target_include_directories (${PLUGIN_NAME} PUBLIC "${CMAKE_SOURCE_DIR}/src/bindings/cpp/include")
		endif (ARG_CPP)
		if (ARG_DEPENDS)
			add_dependencies (${PLUGIN_NAME} ${ARG_DEPENDS})
		endif ()
		if (ARG_LINK_ELEKTRA)
			target_link_libraries (${PLUGIN_NAME} elektra-plugin ${ARG_LINK_ELEKTRA})
		else ()
			target_link_libraries (${PLUGIN_NAME} elektra-plugin)
		endif ()
		target_link_libraries (${PLUGIN_NAME} ${ARG_LINK_LIBRARIES})
		install (
			TARGETS ${PLUGIN_NAME}
			DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}
			COMPONENT "${HAS_COMPONENT}")
		set_property (
			TARGET ${PLUGIN_NAME}
			APPEND
			PROPERTY COMPILE_DEFINITIONS ${ARG_COMPILE_DEFINITIONS} ${ADDITIONAL_COMPILE_DEFINITIONS} "HAVE_KDBCONFIG_H"
				 "PLUGIN_SHORT_NAME=${PLUGIN_SHORT_NAME}" "README=readme_${PLUGIN_SHORT_NAME}.c")
		target_include_directories (${PLUGIN_NAME} PRIVATE "${CMAKE_CURRENT_BINARY_DIR}") # for readme
		set_property (
			TARGET ${PLUGIN_NAME}
			APPEND
			PROPERTY INCLUDE_DIRECTORIES ${ARG_INCLUDE_DIRECTORIES})
		# do not strip rpath during install
		if (ARG_USE_LINK_RPATH)
			set_target_properties (${PLUGIN_NAME} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
		endif ()
		foreach (DIR ${ARG_INCLUDE_SYSTEM_DIRECTORIES})
			if (DIR AND NOT DIR STREQUAL "/usr/include")
				set_property (
					TARGET ${PLUGIN_NAME}
					APPEND_STRING
					PROPERTY COMPILE_FLAGS " ${CMAKE_INCLUDE_SYSTEM_FLAG_CXX} \"${DIR}\"")
			endif ()
		endforeach (DIR)
		if (${LD_ACCEPTS_VERSION_SCRIPT})
			set_property (
				TARGET ${PLUGIN_NAME}
				APPEND
				PROPERTY LINK_FLAGS "-Wl,--version-script=${PROJECT_SOURCE_DIR}/src/plugins/plugin-symbols.map")
		endif ()
	endif ()

	if (NOT ARG_ONLY_SHARED)

		# message (STATUS "added ${PLUGIN_TARGET_OBJS}")
		set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS" ${PLUGIN_TARGET_OBJS} ${ARG_OBJECT_SOURCES})

		set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES" "${ARG_LINK_LIBRARIES}")
	endif (NOT ARG_ONLY_SHARED)

	# cleanup
	unset (ADDITIONAL_COMPILE_DEFINITIONS)
endfunction (add_plugin)
