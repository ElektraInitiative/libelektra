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

# Add a test for a plugin
#
# will include the common tests.h file + its source file
# additional source files can be added as additional arguments
#
# links the executeable (only if build_full)
# and adds a test
macro (add_plugintest testname)
	if (BUILD_FULL)
		set (TEST_SOURCES
				$<TARGET_OBJECTS:cframework>
				${ARGN}
				)
		add_headers(TEST_SOURCES)
		add_testheaders(TEST_SOURCES)
		include_directories ("${CMAKE_SOURCE_DIR}/tests/cframework")
		add_executable (testmod_${testname} ${TEST_SOURCES} testmod_${testname}.c)
		if (INSTALL_TESTING)
			install (TARGETS testmod_${testname}
				DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
		endif (INSTALL_TESTING)
		target_link_libraries (testmod_${testname} elektra-full)
		set_target_properties (testmod_${testname} PROPERTIES
				COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
		add_test (testmod_${testname}
				"${CMAKE_CURRENT_BINARY_DIR}/testmod_${testname}"
				"${CMAKE_CURRENT_SOURCE_DIR}"
				)
	endif (BUILD_FULL)
endmacro (add_plugintest)

# Add a test for cpp plugins
macro (add_cpp_plugintest source)
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
			"${CMAKE_CURRENT_BINARY_DIR}/${source}"
			"${CMAKE_CURRENT_BINARY_DIR}/"
			)
endmacro (add_cpp_plugintest testname)


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

	if (CMAKE_CROSSCOMPILING)
		find_program (EXE_LOC exporterrors)
	else (CMAKE_CROSSCOMPILING)
		get_target_property (EXE_LOC exporterrors LOCATION)
	endif (CMAKE_CROSSCOMPILING)

	add_custom_command (
			OUTPUT ${BINARY_INCLUDE_DIR}/kdberrors.h
			DEPENDS exporterrors
			COMMAND ${EXE_LOC}
			ARGS ${CMAKE_SOURCE_DIR}/src/liberror/specification ${BINARY_INCLUDE_DIR}/kdberrors.h
			)
	list (APPEND ${HDR_FILES} "${BINARY_INCLUDE_DIR}/kdberrors.h")
endmacro (add_headers)

# Add all headers needed for cpp bindings
#
macro (add_cppheaders HDR_FILES)
	include_directories ("${PROJECT_BINARY_DIR}/src/bindings/cpp/include")
	file (GLOB BIN_HDR_FILES ${PROJECT_BINARY_DIR}/src/bindings/cpp/include/*)
	list (APPEND ${HDR_FILES} ${BIN_HDR_FILES})

	include_directories ("${PROJECT_SOURCE_DIR}/src/bindings/cpp/include")
	file (GLOB SRC_HDR_FILES ${PROJECT_SOURCE_DIR}/src/bindings/cpp/include/*)
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
	message ("-- Exclude Plugin ${name} because ${reason}")
	list (REMOVE_ITEM TMP ${name})
	set (PLUGINS ${TMP} CACHE STRING "Which plugins should be compiled?" FORCE)
endmacro (remove_plugin)


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


#- Wrapper of add_library to allow for static modules
#
#  MY_ADD_LIBRARY(<name> [STATIC|SHARED|MODULE]
#                 [EXCLUDE_FROM_ALL]
#                 [STATIC_NAME <static_name>]
#                 <source1> ... <sourceN>)
#
# If STATIC_NAME <static_name> is given, the sources will be appended to
# the global property MY_STATIC_MODULES_<static_name>_SOURCES which can
# then be used with MY_ADD_STATIC_MODULE() to create a static module from
# all of the sources.
#
#Thanks to Michael Wild <themiwi@gmail.com>
#
function(my_add_library name)
	# parse arguments
	set(next_is_static_name FALSE)
	set(static_name)
	set(srcs)
	set(lib_type)
	set(exclude_from_all)
	foreach (src ${ARGN})
		if(arg STREQUAL STATIC_NAME)
			set(next_is_static_name TRUE)
		elseif(arg MATCHES "STATIC|SHARED|MODULE")
			set(lib_type ${arg})
		elseif(arg STREQUAL EXCLUDE_FROM_ALL)
			set(exclude_from_all ${arg})
		elseif(next_is_static_name)
			set(static_name ${arg})
		else()
			# ensure that sources are absolute paths
			if(NOT IS_ABSOLUTE "${arg}")
				get_filename_component(arg "${arg}" ABSOLUTE)
			endif()
			list(APPEND srcs "${arg}")
		endif()
	endforeach (src ${ARGN})
	# require at least one source file
	if (NOT srcs)
		message(SEND_ERROR "At least one source file required")
	endif (NOT srcs)
	# if we have a STATIC_NAME, append the sources to the global property
	if(static_name)
		set(prop_name MY_STATIC_MODULES_${static_name}_SOURCES)
		get_property(prop_defined GLOBAL PROPERTY ${prop_name} DEFINED)
		if(NOT prop_defined)
			define_property(GLOBAL PROPERTY ${prop_name}
					BRIEF_DOCS "Sources for static module ${static_name}"
					FULL_DOCS "Source files to be compiled into the static module ${static_name}")
		endif()
		set_property(GLOBAL APPEND PROPERTY ${prop_name} ${srcs})
	endif()
	# finally, create the normal library
	add_library(${name} ${lib_type} ${exclude_from_all} ${srcs})
endfunction()


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
	FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/readme_${p}.c ${contents})
endfunction()
