# Copy a file from source dir to binary dir
#
# copy_file name
#
macro (copy_file filename)
	execute_process (
			COMMAND
			${CMAKE_COMMAND} -E copy
				"${CMAKE_CURRENT_SOURCE_DIR}/${filename}.h"
				"${CMAKE_CURRENT_BINARY_DIR}/${filename}.h"
		)
endmacro (copy_file)

macro (add_headers HDR_FILES)
	include_directories ("${PROJECT_BINARY_DIR}/src/include")
	file (GLOB BIN_HDR_FILES ${PROJECT_BINARY_DIR}/src/include/*.h)
	list (APPEND HDR_FILES ${BIN_HDR_FILES})

	include_directories ("${PROJECT_SOURCE_DIR}/src/include")
	file (GLOB SRC_HDR_FILES ${PROJECT_SOURCE_DIR}/src/include/*.h)
	list (APPEND HDR_FILES ${SRC_HDR_FILES})
endmacro (add_headers)


#- Add sources for a target
#
#  ADD_SOURCES(<target> <source1> [<source2> ...])
#
function(add_sources target)
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
		message (added: ${src})
	endforeach ()
	# append to global property
	set_property (GLOBAL APPEND PROPERTY "${target}_SRCS" "${SRCS}")
endfunction(add_sources)


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
function(my_add_library name)
	# parse arguments
	set(next_is_static_name FALSE)
	set(static_name)
	set(srcs)
	set(lib_type)
	set(exclude_from_all)
	foreach(arg IN LISTS ARGN)
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
	endforeach()
	# require at least one source file
	if(NOT srcs)
		message(SEND_ERROR "At least one source file required")
	endif()
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
function(my_add_static_module name)
	get_property(srcs GLOBAL PROPERTY
			MY_STATIC_MODULES_${name}_SOURCES)
	if(not SRCS)
		message(SEND_ERROR "No sources defined for static module ${name}")
	endif()
	add_library(${name} STATIC ${srcs})
endfunction()

