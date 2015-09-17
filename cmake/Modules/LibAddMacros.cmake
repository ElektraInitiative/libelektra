# Copy a file from source dir to binary dir
#
# copy_file or directory
#
macro (copy_file src dest)
	execute_process (
			COMMAND
			${CMAKE_COMMAND} -E copy
				"${src}"
				"${dest}"
		)
endmacro (copy_file)


# Create a symlink
#create_symlink old new    - create a symbolic link new -> old
#
macro (create_symlink old new)
	execute_process (
			COMMAND
			${CMAKE_COMMAND} -E create_symlink
				"${old}"
				"${new}"
		)
endmacro (create_symlink)

# Make a directory
#
# mkdir dir
#
macro (mkdir dir)
	execute_process (
			COMMAND
			${CMAKE_COMMAND} -E make_directory
				"${dir}"
		)
endmacro (mkdir)

macro (add_testheaders HDR_FILES)
	include_directories ("${PROJECT_SOURCE_DIR}/tests/cframework")
	file (GLOB BIN_HDR_FILES
		"${PROJECT_SOURCE_DIR}/tests/cframework/*.h")
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})
endmacro (add_testheaders HDR_FILES)

# for generic targets (not tools) use this function to link against elektra
function (target_link_elektra TARGET)
	if (BUILD_FULL)
		target_link_libraries (${TARGET} elektra-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektra-static)
	elseif (BUILD_SHARED)
		target_link_libraries (${TARGET} elektra)
	else ()
		message(SEND_ERROR "no elektra to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()

endfunction()

function (target_link_elektratools TARGET)
	if (BUILD_FULL)
		target_link_libraries (${TARGET} elektratools-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektratools-static)
	elseif (BUILD_SHARED)
		target_link_libraries (${TARGET} elektratools)
	else ()
		message(SEND_ERROR "no elektratools to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()

endfunction()

# for tools (not tests) use this function to link against elektra
macro(tool_link_elektra TARGET)
	if (BUILD_SHARED)
		target_link_libraries (${TARGET} elektra)
	elseif (BUILD_FULL)
		target_link_libraries (${TARGET} elektra-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektra-static)
	else ()
		message(SEND_ERROR "no elektra to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()
endmacro()

macro(tool_link_elektratools TARGET)
	if (BUILD_SHARED)
		target_link_libraries (${TARGET} elektratools)
	elseif (BUILD_FULL)
		target_link_libraries (${TARGET} elektratools-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektratools-static)
	else ()
		message(SEND_ERROR "no elektratools to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()
endmacro()


# Add a test for a plugin
#
# will include the common tests.h file + its source file
# additional source files can be added as additional arguments
#
# links the executeable (only if build_full)
# and adds a test
macro (add_plugintest testname)
	if (BUILD_TESTING AND (BUILD_STATIC OR BUILD_FULL))
		cmake_parse_arguments (ARG
			"MEMLEAK" # optional keywords
			""        # one value keywords
			"COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES;LINK_LIBRARIES;WORKING_DIRECTORY" # multi value keywords
			${ARGN}
		)
		set (TEST_SOURCES
				$<TARGET_OBJECTS:cframework>
				${ARG_UNPARSED_ARGUMENTS}
				)
		add_headers(TEST_SOURCES)
		add_testheaders(TEST_SOURCES)
		include_directories ("${CMAKE_SOURCE_DIR}/tests/cframework")
		add_executable (testmod_${testname} ${TEST_SOURCES} testmod_${testname}.c)
		if (INSTALL_TESTING)
			install (TARGETS testmod_${testname}
				DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
		endif (INSTALL_TESTING)
		target_link_elektra(testmod_${testname})
		target_link_libraries (testmod_${testname} ${ARG_LINK_LIBRARIES})
		set_target_properties (testmod_${testname} PROPERTIES
				COMPILE_DEFINITIONS "HAVE_KDBCONFIG_H;${ARG_COMPILE_DEFINITIONS}")
		set_property(TARGET testmod_${testname}
				APPEND PROPERTY INCLUDE_DIRECTORIES
				${ARG_INCLUDE_DIRECTORIES})
		add_test (NAME testmod_${testname}
				COMMAND "${CMAKE_BINARY_DIR}/bin/testmod_${testname}" "${CMAKE_CURRENT_SOURCE_DIR}"
				WORKING_DIRECTORY ${ARG_WORKING_DIRECTORY}
				)
		if (ARG_MEMLEAK)
			set_property(TEST testmod_${testname} PROPERTY
				LABELS memleak)
		endif (ARG_MEMLEAK)
	endif ()
endmacro (add_plugintest)

# Add a test for cpp plugins
macro (add_cpp_plugintest testname)
	if (BUILD_TESTING)
		cmake_parse_arguments (ARG
			"MEMLEAK" # optional keywords
			""        # one value keywords
			""        # multi value keywords
			${ARGN}
		)
		set (source "testmod_${testname}")
		include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")
		include_directories ("${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests")
		set (SOURCES ${HDR_FILES} ${source}.cpp ${CMAKE_SOURCE_DIR}/src/bindings/cpp/tests/tests.cpp)
		add_executable (${source} ${SOURCES})

		if (BUILD_FULL)
			target_link_libraries (${source} elektra-full)
		else (BUILD_FULL)
			target_link_libraries (${source} elektra-static)
		endif (BUILD_FULL)

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
			set_property(TEST testmod_${testname} PROPERTY
				LABELS memleak)
		endif (ARG_MEMLEAK)
	endif()
endmacro (add_cpp_plugintest testname)

macro(find_swig)
	if (NOT SWIG_FOUND)
		find_package(SWIG 3)
		if (NOT SWIG_FOUND)
			message(STATUS "Search for swig2 instead")
			find_package(SWIG 2 QUIET)
		endif()
	endif (NOT SWIG_FOUND)
endmacro(find_swig)


function(find_util util output_loc output_arg)
	if (CMAKE_CROSSCOMPILING)
		if (WIN32)
			find_program(EXE_LOC wine)
			if (EXE_LOC)
				set (ARG_LOC "${CMAKE_BINARY_DIR}/bin/${util}.exe")
			else()
				find_program (EXE_LOC
					HINTS
						${CMAKE_BINARY_DIR}
					${util}.exe)
			endif ()
		else()
			find_program (EXE_LOC ${util})
		endif ()
	else (CMAKE_CROSSCOMPILING)
		get_target_property (EXE_LOC ${util} LOCATION)
	endif (CMAKE_CROSSCOMPILING)
	set (${output_loc} ${EXE_LOC} PARENT_SCOPE)
	set (${output_arg} ${ARG_LOC} PARENT_SCOPE)
endfunction(find_util util output)


#- Adds all headerfiles of global include path to the given variable
#
#  ADD_HEADERS (variable)
#
# example:
#add_headers (SOURCES)
#SOURCES now contain the names of all global header files
#
# thus the necessary directories are also included within the macro
# don't execute this within a loop.
#
# The added files will be always the same anyway. Example see in
# tests/CMakeLists.txt
#
macro (add_headers HDR_FILES)
	set (BINARY_INCLUDE_DIR "${PROJECT_BINARY_DIR}/src/include")
	set (SOURCE_INCLUDE_DIR "${PROJECT_SOURCE_DIR}/src/include")

	include_directories (BEFORE "${BINARY_INCLUDE_DIR}")
	file (GLOB BIN_HDR_FILES ${BINARY_INCLUDE_DIR}/*.h)
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})

	include_directories (AFTER ${SOURCE_INCLUDE_DIR})
	file (GLOB SRC_HDR_FILES ${SOURCE_INCLUDE_DIR}/*.h)
	list (APPEND ${HDR_FILES} ${SRC_HDR_FILES})

	find_util(exporterrors EXE_ERR_LOC EXE_ERR_ARG)

	add_custom_command (
			OUTPUT ${BINARY_INCLUDE_DIR}/kdberrors.h
			DEPENDS exporterrors
			COMMAND ${EXE_ERR_LOC}
			ARGS ${EXE_ERR_ARG} ${CMAKE_SOURCE_DIR}/src/liberror/specification ${BINARY_INCLUDE_DIR}/kdberrors.h
			)
	list (APPEND ${HDR_FILES} "${BINARY_INCLUDE_DIR}/kdberrors.h")
endmacro (add_headers)

# Add all headers needed for cpp bindings
#
macro (add_cppheaders HDR_FILES)
	include_directories ("${PROJECT_BINARY_DIR}/src/bindings/cpp/include")
	file (GLOB BIN_HDR_FILES ${PROJECT_BINARY_DIR}/src/bindings/cpp/include/*.hpp)
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})

	include_directories ("${PROJECT_SOURCE_DIR}/src/bindings/cpp/include")
	file (GLOB SRC_HDR_FILES ${PROJECT_SOURCE_DIR}/src/bindings/cpp/include/*.hpp)
	list (APPEND ${HDR_FILES} ${SRC_HDR_FILES})
endmacro (add_cppheaders)

macro (add_toolheaders HDR_FILES)
	include_directories ("${PROJECT_BINARY_DIR}/src/libtools/include")
	file (GLOB BIN_HDR_FILES ${PROJECT_BINARY_DIR}/src/libtools/include/*)
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})

	include_directories ("${PROJECT_SOURCE_DIR}/src/libtools/include")
	file (GLOB SRC_HDR_FILES ${PROJECT_SOURCE_DIR}/src/libtools/include/*)
	list (APPEND ${HDR_FILES} ${SRC_HDR_FILES})
endmacro (add_toolheaders)


#- Removes a plugin from the global cache
#
#  REMOVE_PLUGIN (name reason)
#
# example:
#remove_plugin (fstab "mntent is missing")
#
macro (remove_plugin name reason)
	set (TMP ${PLUGINS})
	message (STATUS "Exclude Plugin ${name} because ${reason}")
	list (REMOVE_ITEM TMP ${name})
	set (PLUGINS ${TMP} CACHE STRING ${PLUGINS_DOC} FORCE)
endmacro (remove_plugin)


macro (remove_binding name reason)
	set (TMP ${BINDINGS})
	message (STATUS "Exclude Binding ${name} because ${reason}")
	list (REMOVE_ITEM TMP ${name})
	set (BINDINGS ${TMP} CACHE STRING ${BINDINGS_DOC} FORCE)
endmacro (remove_binding)


macro (remove_tool name reason)
	set (TMP ${TOOLS})
	message (STATUS "Exclude tool ${name} because ${reason}")
	list (REMOVE_ITEM TMP ${name})
	set (TOOLS ${TMP} CACHE STRING ${TOOLS_DOC} FORCE)
endmacro (remove_tool)

# LIST_FILTER(<list> <regexp_var> [<regexp_var> ...]
#              [OUTPUT_VARIABLE <variable>])
# Removes items from <list> which match any of the specified
# regular expressions. An optional argument OUTPUT_VARIABLE
# specifies a variable in which to store the matched items instead of
# updating <list>
# As regular expressions can not be given to macros (see bug #5389), we pass
# variable names whose content is the regular expressions.
#
# copied from http://www.cmake.org/Wiki/CMakeMacroListOperations
MACRO(list_filter)
  cmake_parse_arguments(LIST_FILTER "" "OUTPUT_VARIABLE" "" ${ARGV})
  # Check arguments.
  LIST(LENGTH LIST_FILTER_DEFAULT_ARGS LIST_FILTER_default_length)
  IF(${LIST_FILTER_default_length} EQUAL 0)
    MESSAGE(FATAL_ERROR "LIST_FILTER: missing list variable.")
  ENDIF(${LIST_FILTER_default_length} EQUAL 0)
  IF(${LIST_FILTER_default_length} EQUAL 1)
    MESSAGE(FATAL_ERROR "LIST_FILTER: missing regular expression variable.")
  ENDIF(${LIST_FILTER_default_length} EQUAL 1)
  # Reset output variable
  IF(NOT LIST_FILTER_OUTPUT_VARIABLE)
    SET(LIST_FILTER_OUTPUT_VARIABLE "LIST_FILTER_internal_output")
  ENDIF(NOT LIST_FILTER_OUTPUT_VARIABLE)
  SET(${LIST_FILTER_OUTPUT_VARIABLE})
  # Extract input list from arguments
  LIST(GET LIST_FILTER_DEFAULT_ARGS 0 LIST_FILTER_input_list)
  LIST(REMOVE_AT LIST_FILTER_DEFAULT_ARGS 0)
  FOREACH(LIST_FILTER_item ${${LIST_FILTER_input_list}})
    set(add_item "1")
    FOREACH(LIST_FILTER_regexp_var ${LIST_FILTER_DEFAULT_ARGS})
      FOREACH(LIST_FILTER_regexp ${${LIST_FILTER_regexp_var}})
        IF(${LIST_FILTER_item} MATCHES ${LIST_FILTER_regexp})
          set(add_item "0")
        ENDIF(${LIST_FILTER_item} MATCHES ${LIST_FILTER_regexp})
      ENDFOREACH(LIST_FILTER_regexp ${${LIST_FILTER_regexp_var}})
    ENDFOREACH(LIST_FILTER_regexp_var)
    if (add_item)
      LIST(APPEND ${LIST_FILTER_OUTPUT_VARIABLE} ${LIST_FILTER_item})
    endif()
  ENDFOREACH(LIST_FILTER_item)
  # If OUTPUT_VARIABLE is not specified, overwrite the input list.
  IF(${LIST_FILTER_OUTPUT_VARIABLE} STREQUAL "LIST_FILTER_internal_output")
    SET(${LIST_FILTER_input_list} ${${LIST_FILTER_OUTPUT_VARIABLE}})
  ENDIF(${LIST_FILTER_OUTPUT_VARIABLE} STREQUAL "LIST_FILTER_internal_output")
ENDMACRO(list_filter)

#find string in list with regex
function(list_find input_list regexp_var output)
  # Reset output variable
  # Extract input list from arguments
  set(${output} "0" PARENT_SCOPE)
  FOREACH(LIST_FILTER_item ${${input_list}})
    FOREACH(LIST_FILTER_regexp ${${regexp_var}})
      #message("try to match ${LIST_FILTER_regexp} with ${LIST_FILTER_item}")
      IF(${LIST_FILTER_item} MATCHES ${LIST_FILTER_regexp})
        set(${output} "1" PARENT_SCOPE)
      ENDIF(${LIST_FILTER_item} MATCHES ${LIST_FILTER_regexp})
    ENDFOREACH(LIST_FILTER_regexp ${regexp_var})
  ENDFOREACH(LIST_FILTER_item)
endfunction(list_find)



#- Add sources for a target
#
#  ADD_SOURCES (<target> <source1> [<source2> ...])
#
# The target should add the sources using:
#
#get_property (elektra_SRCS GLOBAL PROPERTY elektra_SRCS)
#list (APPEND SRC_FILES ${elektra_SRCS})
#
## descend into sub-directories
#add_subdirectory(a)
#add_subdirectory(b)
#
#get_property(super_SRCS GLOBAL PROPERTY super_SRCS)
#
#add_library(super STATIC ${super_SRCS})
################
#
#a/CMakeLists.txt:
##################
#add_sources(super
#  a1.f
#  a2.f
#  a3.f
#  )
##################
#
#b/CMakeLists.txt:
##################
#add_sources(super
#  b1.f
#  b2.f
#  )
##################
#
#Thank to Michael Wild
#
#
# elektra...        are the sources for all elektra targets
# elektra-shared... are the additional sources
#                   for elektra SHARED only (excludes elektra-full)
# elektra-full...   are the additional sources for the versions
#                   with all plugins built-in (excludes elektra-shared)
#
function (add_sources target)
	# define the <target>_SRCS properties if necessary
	get_property (prop_defined GLOBAL PROPERTY ${target}_SRCS DEFINED)
	if (NOT prop_defined)
		define_property (GLOBAL PROPERTY ${target}_SRCS
			BRIEF_DOCS "Sources for the ${target} target"
			FULL_DOCS "List of source files for the ${target} target")
	endif (NOT prop_defined)
	# create list of sources (absolute paths)
	set (SRCS)
	foreach (src ${ARGN})
		if (NOT IS_ABSOLUTE "${src}")
			get_filename_component (src "${src}" ABSOLUTE)
		endif (NOT IS_ABSOLUTE "${src}")
		list (APPEND SRCS "${src}")
	endforeach (src ${ARGN})
	# append to global property
	set_property (GLOBAL APPEND PROPERTY "${target}_SRCS" "${SRCS}")
endfunction (add_sources)

#- Add includes for a target
#
#  ADD_INCLUDES (<target> <source1> [<source2> ...])
#
# The target should do:
#
#get_property (elektra_INCLUDES GLOBAL PROPERTY elektra_INCLUDES)
#include_directories (${elektra_INCLUDES})
#
function (add_includes target)
	# define the <target>_INCLUDES properties if necessary
	get_property (prop_defined GLOBAL PROPERTY ${target}_INCLUDES DEFINED)
	if (NOT prop_defined)
		define_property (GLOBAL PROPERTY ${target}_INCLUDES
			BRIEF_DOCS "Sources for the ${target} target"
			FULL_DOCS "List of source files for the ${target} target")
	endif (NOT prop_defined)
	# create list of sources (absolute paths)
	set (INCLUDES)
	foreach (src ${ARGN})
		list (APPEND INCLUDES "${src}")
	endforeach (src ${ARGN})
	# append to global property
	set_property (GLOBAL APPEND PROPERTY "${target}_INCLUDES" "${INCLUDES}")
endfunction (add_includes)


#- Add libraries for a target
#
#  ADD_LIBRARIES (<target> <source1> [<source2> ...])
#
# The target should do:
#
#get_property (elektra_LIBRARIES GLOBAL PROPERTY elektra_LIBRARIES)
#target_link_libraries (elektra ${elektra_LIBRARIES})
#
function (add_libraries target)
	# define the <target>_LIBRARIES properties if necessary
	get_property (prop_defined GLOBAL PROPERTY ${target}_LIBRARIES DEFINED)
	if (NOT prop_defined)
		define_property (GLOBAL PROPERTY ${target}_LIBRARIES
			BRIEF_DOCS "Sources for the ${target} target"
			FULL_DOCS "List of source files for the ${target} target")
	endif (NOT prop_defined)
	# create list of sources (absolute paths)
	set (LIBRARIES)
	foreach (src ${ARGN})
		list (APPEND LIBRARIES "${src}")
	endforeach (src ${ARGN})
	# append to global property
	set_property (GLOBAL APPEND PROPERTY "${target}_LIBRARIES" "${LIBRARIES}")
endfunction (add_libraries)


#- Create a static library from sources collected by MY_ADD_LIBRARY
#
#  MY_ADD_STATIC_MODULE(<name>)
#
# Compiles a static library <name> from the sources defined in
# MY_STATIC_MODULES_<name>_SOURCES by MY_ADD_LIBRARY(). This
# function must be called after all calls to MY_ADD_LIBRARY()
# contributing sources to this static module.
#
#Thanks to Michael Wild <themiwi@gmail.com>
#
function(my_add_static_module name)
	get_property(srcs GLOBAL PROPERTY
			MY_STATIC_MODULES_${name}_SOURCES)
	if (not SRCS)
		message(SEND_ERROR "No sources defined for static module ${name}")
	endif (not SRCS)
	add_library(${name} STATIC ${srcs})
endfunction()

macro(remember_for_removal ELEMENTS TO_REMOVE_ELEMENTS)
	set (MY_ELEMENTS ${${ELEMENTS}})
	set (MY_REMOVE_ELEMENTS "")
	foreach(B ${MY_ELEMENTS})
		if(B MATCHES "^-.*")
			## remove pseudo "-element"
			list(REMOVE_ITEM MY_ELEMENTS ${B})
			string(LENGTH ${B} B_LENGTH)
			math(EXPR L ${B_LENGTH}-1)
			string(SUBSTRING ${B} 1 ${L} B_OUT)
			list(APPEND MY_REMOVE_ELEMENTS ${B_OUT})
		endif()
	endforeach(B)
	set(${TO_REMOVE_ELEMENTS} ${MY_REMOVE_ELEMENTS})
	set(${ELEMENTS} ${MY_ELEMENTS})
endmacro()

macro(removal ELEMENTS TO_REMOVE_ELEMENTS)
	set(MY_ELEMENTS ${${ELEMENTS}})
	list(REMOVE_DUPLICATES MY_ELEMENTS)
	foreach(B ${${TO_REMOVE_ELEMENTS}})
		list(REMOVE_ITEM MY_ELEMENTS ${B})
	endforeach(B)
	set(${ELEMENTS} ${MY_ELEMENTS})
endmacro()


#
# Parameter: the pluginname
#
# converts a README.md (markdown texts with key/value pairs)
# to a readme_pluginname.c that contains keys to be added in the contract
#
# the key/value pairs need to be written like:
# - infos/licence = BSD
#
# - infos/description =
# has a special functionality: when it is used the rest of the file
# is interpreted as description. So this key must be last.
#
function (generate_readme p)
	# rerun cmake when README.md is changed
	# also allows cmake variable substitution
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/README.md ${CMAKE_CURRENT_BINARY_DIR}/README.out)

	# read
	FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/README.out contents)
	STRING(REGEX REPLACE "\\\\" "\\\\\\\\" contents "${contents}")
	STRING(REGEX REPLACE "\"" "\\\\\"" contents "${contents}")
	STRING(REGEX REPLACE "\n" "\\\\n\"\n\"" contents "${contents}")
	STRING(REGEX REPLACE "- infos = ([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/licence *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/licence\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/author *= *([.@<>a-zA-Z0-9 %_-]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/author\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/provides *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/provides\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/placements *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/placements\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/recommends *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/recommends\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/ordering *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/ordering\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/needs *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/needs\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/description *= *(.*)\\\\n\"\n\"" "keyNew(\"system/elektra/modules/${p}/infos/description\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	# allow macros:
	STRING(REGEX REPLACE "\" *#ifdef ([^\\]*)\\\\n\"" "#ifdef \\1" contents "${contents}")
	STRING(REGEX REPLACE "\" *#endif\\\\n\"" "#endif" contents "${contents}")
	FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/readme_${p}.c "${contents}\n")
endfunction()
