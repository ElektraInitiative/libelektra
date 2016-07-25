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


# Create a symlink for a lib/plugin both in lib and at installation
#
# create_symlink src dest - create a symbolic link from src -> dest
#
macro (create_lib_symlink src dest)
	execute_process (COMMAND ${CMAKE_COMMAND} -E create_symlink
		"${src}"
		"${dest}"
		WORKING_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
		)

	set (LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}")

	install(CODE "
		message (STATUS \"Installing symlink: \$ENV{DESTDIR}${LIB_INSTALL_DIR}/${dest} -> ${src}\")
		execute_process (COMMAND \"${CMAKE_COMMAND}\" -E make_directory
			\"\$ENV{DESTDIR}${LIB_INSTALL_DIR}\"
			RESULT_VARIABLE RET
			)
		if (RET)
			message (WARNING \"Could not create directory\")
		endif ()
		execute_process (COMMAND \"${CMAKE_COMMAND}\" -E create_symlink
			\"${src}\"
			\"${dest}\"
			WORKING_DIRECTORY \"\$ENV{DESTDIR}${LIB_INSTALL_DIR}\"
			RESULT_VARIABLE RET
			)
		if (RET)
			message (WARNING \"Could not install symlink\")
		endif ()
		")
endmacro (create_lib_symlink src dest)

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
	if (BUILD_SHARED)
		target_link_libraries (${TARGET} elektra-core ${ARGN})
	elseif (BUILD_FULL)
		target_link_libraries (${TARGET} elektra-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektra-static)
	else ()
		message(SEND_ERROR "no elektra to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()

endfunction()

function (target_link_elektratools TARGET)
	if (BUILD_SHARED)
		target_link_libraries (${TARGET} elektratools elektra-kdb elektra-core ${ARGN})
	elseif (BUILD_FULL)
		target_link_libraries (${TARGET} elektratools-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektratools-static)
	else ()
		message(SEND_ERROR "no elektratools to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()

endfunction()

# for tools (not tests) use this function to link against elektra
macro(tool_link_elektra TARGET)
	if (BUILD_SHARED)
		target_link_libraries (${TARGET} elektra-core elektra-kdb ${ARGN})
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
		target_link_libraries (${TARGET} elektratools ${ARGN})
	elseif (BUILD_FULL)
		target_link_libraries (${TARGET} elektratools-full)
	elseif (BUILD_STATIC)
		target_link_libraries (${TARGET} elektratools-static)
	else ()
		message(SEND_ERROR "no elektratools to link for ${TARGET}, please enable BUILD_FULL, BUILD_STATIC or BUILD_SHARED")
	endif ()
endmacro()


macro(find_swig)
	if (NOT SWIG_FOUND)
		find_package(SWIG 3)
		if (NOT SWIG_FOUND)
			message(STATUS "Search for swig2 instead")
			find_package(SWIG 2 QUIET)
		endif()
	endif (NOT SWIG_FOUND)
endmacro(find_swig)

#
# find an util to pass to add_custom_command later
#
# Parameters:
#
# 1. util        [in] : the utility fo search for. Must be added using
#                       add_executable first.
# 2. EXE_SYM_LOC [out]: a name for a variable where the program to be executed
#                       is written to.
#                       Note: you must pass this value to COMMAND of add_custom_command
# 3. EXE_SYM_ARG [out]: a name for a variable where the argument to be executed
#                       is written to.
#                       Note: you must pass this value to ARGS in add_custom_command
#
# Example Usage:
# add_executable (elektra-export-symbols ...)
# include(LibAddMacros)
# find_util(elektra-export-symbols EXE_SYM_LOC EXE_SYM_ARG)
#
# add_custom_command (
#		DEPENDS elektra-export-symbols
#		COMMAND ${EXE_SYM_LOC}
#		ARGS ${EXE_SYM_ARG} ... other arguments
#		)
#
function(find_util util output_loc output_arg)
	if (CMAKE_CROSSCOMPILING)
		if (WIN32)
			find_program(${util}_EXE_LOC wine)
			if (${util}_EXE_LOC)
				set (ARG_LOC "${CMAKE_BINARY_DIR}/bin/${util}.exe")
			else()
				find_program (${util}_EXE_LOC
					HINTS
						${CMAKE_BINARY_DIR}
					${util}.exe)
			endif ()
		else()
			find_program (${util}_EXE_LOC ${util})
		endif ()
	else (CMAKE_CROSSCOMPILING)
		get_target_property (${util}_EXE_LOC ${util} LOCATION)
	endif (CMAKE_CROSSCOMPILING)
	set (${output_loc} ${${util}_EXE_LOC} PARENT_SCOPE)
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

	set_source_files_properties (${BINARY_INCLUDE_DIR}/kdberrors.h PROPERTIES GENERATED ON)
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
	include_directories ("${PROJECT_BINARY_DIR}/src/libs/tools/include")
	file (GLOB BIN_HDR_FILES ${PROJECT_BINARY_DIR}/src/libtools/include/*)
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})

	include_directories ("${PROJECT_SOURCE_DIR}/src/libs/tools/include")
	file (GLOB SRC_HDR_FILES ${PROJECT_SOURCE_DIR}/src/libs/tools/include/*)
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
	if (NOT ${reason} STREQUAL "silent")
		message (STATUS "Exclude Plugin ${name} because ${reason}")
	endif ()

	if (ADDED_PLUGINS)
		set (TMP ${ADDED_PLUGINS})
		list (REMOVE_ITEM TMP ${name})
		set (ADDED_PLUGINS ${TMP} CACHE STRING "${ADDED_PLUGINS_DOC}" FORCE)
	endif (ADDED_PLUGINS)

	if (ADDED_DIRECTORIES)
		set (TMP ${ADDED_DIRECTORIES})
		list (REMOVE_ITEM TMP ${name})
		set (ADDED_DIRECTORIES ${TMP} CACHE STRING "${ADDED_DIRECTORIES_DOC}" FORCE)
	endif (ADDED_DIRECTORIES)

	if (REMOVED_PLUGINS)
		set (REMOVED_PLUGINS "${REMOVED_PLUGINS};${name}" CACHE STRING "${REMOVED_PLUGINS_DOC}" FORCE)
	else ()
		set (REMOVED_PLUGINS "${name}" CACHE STRING "${REMOVED_PLUGINS_DOC}" FORCE)
	endif ()
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

# LIST_FILTER(<list> <regexp_var>
# Removes items from <list> which match the specified
# regular expression. An optional argument OUTPUT_VARIABLE
# specifies a variable in which to store the matched items instead of
# updating <list>
# As regular expressions can not be given to macros (see bug #5389), we pass
# variable names whose content is the regular expressions.
#
# For example:
#set (XXX "gi_a;gi_b;swig_a;gi_c;swig_d;x;y")
#set (YYY "swig_.*")
#list_filter(XXX YYY)
#message (STATUS "XXX is ${XXX}") will be gi_a;gi_b;gi_c;x;y
#
function(list_filter result regex)
	set(newlist)
	foreach(r ${${result}})
		if(r MATCHES ${${regex}})
		else()
			list(APPEND newlist ${r})
		endif()
	endforeach()
	set(${result} ${newlist} PARENT_SCOPE)
endfunction()

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
		if (B MATCHES "^-.*")
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
	if (${ELEMENTS})
		set (MY_ELEMENTS ${${ELEMENTS}})
		list (REMOVE_DUPLICATES MY_ELEMENTS)
		foreach (B ${${TO_REMOVE_ELEMENTS}})
			list (REMOVE_ITEM MY_ELEMENTS ${B})
		endforeach (B)
		set (${ELEMENTS} ${MY_ELEMENTS})
	endif ()
endmacro()


function (generate_manpage NAME)
if (BUILD_DOCUMENTATION AND RONN_LOC) # disable function when RONN_LOC is not set

	cmake_parse_arguments (ARG
		"" # optional keywords
		"SECTION;FILENAME" # one value keywords
		"" # multi value keywords
		${ARGN}
	)

	if (ARG_SECTION)
		set(SECTION ${ARG_SECTION})
	else ()
		set(SECTION 1)
	endif ()

	if (ARG_FILENAME)
		set(MDFILE ${ARG_FILENAME})
	else ()
		set(MDFILE ${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.md)
	endif ()

	set(OUTFILE ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.${SECTION})

	add_custom_command(
		OUTPUT ${OUTFILE}
		DEPENDS ${MDFILE}
		COMMAND ${RONN_LOC}
		ARGS -r --pipe ${MDFILE} > ${OUTFILE}
		)
	add_custom_target(man-${NAME} ALL DEPENDS ${OUTFILE})
	add_dependencies(man man-${NAME})

	if (INSTALL_DOCUMENTATION)
		install(
			FILES ${OUTFILE}
			DESTINATION share/man/man${SECTION}
			)
	endif ()
endif (BUILD_DOCUMENTATION AND RONN_LOC)
endfunction ()


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
	STRING(REGEX REPLACE "\"- +infos/stacking *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/stacking\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/needs *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/needs\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	if (p STREQUAL ${KDB_DEFAULT_STORAGE} OR p STREQUAL KDB_DEFAULT_RESOLVER)
	STRING(REGEX REPLACE "\"- +infos/status *= *([-a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/status\",\nKEY_VALUE, \"\\1 default\", KEY_END)," contents "${contents}")
	else ()
	STRING(REGEX REPLACE "\"- +infos/status *= *([-a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/status\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	endif ()
	STRING(REGEX REPLACE "\"- +infos/metadata *= *([/a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/metadata\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/plugins *= *([a-zA-Z0-9 ]*)\\\\n\"" "keyNew(\"system/elektra/modules/${p}/infos/plugins\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	STRING(REGEX REPLACE "\"- +infos/description *= *(.*)\\\\n\"\n\"" "keyNew(\"system/elektra/modules/${p}/infos/description\",\nKEY_VALUE, \"\\1\", KEY_END)," contents "${contents}")
	# allow macros:
	STRING(REGEX REPLACE "\" *#ifdef ([^\\]*)\\\\n\"" "#ifdef \\1" contents "${contents}")
	STRING(REGEX REPLACE "\" *#ifndef ([^\\]*)\\\\n\"" "#ifndef \\1" contents "${contents}")
	STRING(REGEX REPLACE "\" *#else\\\\n\"" "#else" contents "${contents}")
	STRING(REGEX REPLACE "\" *#endif\\\\n\"" "#endif" contents "${contents}")
	FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/readme_${p}.c "${contents}\n")
endfunction()
