include(LibAddMacros)

# add_plugin: add a plugin to Elektra
#
# SOURCES:
#  The sources of the plugin
#
# SHARED_SOURCES:
#  Will be added only once without any per-variant macros nor
#  COMPILE_DEFINITIONS.
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
function(add_plugin PLUGIN_SHORT_NAME)
	cmake_parse_arguments (ARG
		"CPP" # optional keywords
		"" # one value keywords
		"SOURCES;SHARED_SOURCES;LINK_LIBRARIES;COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES;LINK_ELEKTRA" # multi value keywords
		${ARGN}
	)

	set (PLUGIN_NAME elektra-${PLUGIN_SHORT_NAME})
	set (PLUGIN_OBJS ${PLUGIN_NAME}-objects)
	set (PLUGIN_TARGET_OBJS "$<TARGET_OBJECTS:${PLUGIN_OBJS}>")
	file(GLOB PLUGIN_SHARED_SOURCES ${ARG_SHARED_SOURCES})

	#message (STATUS "name: ${PLUGIN_NAME}")
	#message (STATUS "srcs are: ${ARG_SOURCES}")
	#message (STATUS "deps are: ${ARG_LINK_LIBRARIES}")
	#message (STATUS "comp are: ${ARG_COMPILE_DEFINITIONS}")
	#message (STATUS "incl are: ${ARG_INCLUDE_DIRECTORIES}")
	#message (STATUS "current bin ${CMAKE_CURRENT_BINARY_DIR}")

	add_headers(ARG_SOURCES)
	if (ARG_CPP)
		add_cppheaders(ARG_SOURCES)
	endif (ARG_CPP)

	add_library (${PLUGIN_OBJS} OBJECT ${ARG_SOURCES})

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
		add_library (${PLUGIN_NAME} MODULE ${ARG_SOURCES}
			${PLUGIN_SHARED_SOURCES})
		if (ARG_LINK_ELEKTRA)
			target_link_libraries (${PLUGIN_NAME} ${ARG_LINK_ELEKTRA})
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
	endif()

	set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS"
		${PLUGIN_TARGET_OBJS}
		${PLUGIN_SHARED_SOURCES}
		)

	set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES"
		"${ARG_LINK_LIBRARIES}"
		)
endfunction()
