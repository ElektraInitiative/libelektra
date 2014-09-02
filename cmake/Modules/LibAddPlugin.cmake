include(LibParseArguments)
include(LibAddMacros)

function(add_plugin_helper HELPER_NAME)
	parse_arguments(ARG
		"SOURCES;LINK_LIBRARIES;COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES"
		"" # no option
		${ARGN}
		)

	add_headers(ARG_SOURCES)
	add_library (${HELPER_NAME} STATIC ${ARG_SOURCES})

	set_property(TARGET ${HELPER_NAME}
		APPEND PROPERTY COMPILE_DEFINITIONS
		${ARG_COMPILE_DEFINITIONS}
		"HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
		)

	set_property(TARGET ${HELPER_NAME}
		APPEND PROPERTY INCLUDE_DIRECTORIES
		${ARG_INCLUDE_DIRECTORIES}
		${CMAKE_BINARY_DIR}/src/include
		${CMAKE_SOURCE_DIR}/src/include
		)

	set_property(TARGET ${HELPER_NAME}
		APPEND PROPERTY COMPILE_FLAGS
		${CMAKE_PIC_FLAGS}) # needed for shared libraries

	# needs cmake 3.0:
	#set_property(TARGET ${PLUGIN_OBJS}
	#	PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)
endfunction()

# do not add elektra as library
# only add libraries found by cmake and helper libraries created with
# add_plugin_helper
function(add_plugin PLUGIN_SHORT_NAME)
	parse_arguments(ARG
		"SOURCES;LINK_LIBRARIES;COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES"
		"" # no option
		${ARGN}
		)


	set (PLUGIN_NAME elektra-${PLUGIN_SHORT_NAME})
	set (PLUGIN_OBJS ${PLUGIN_NAME}-objects)

	#message (STATUS "name: ${PLUGIN_NAME}")
	#message (STATUS "srcs are: ${ARG_SOURCES}")
	#message (STATUS "deps are: ${ARG_LINK_LIBRARIES}")
	#message (STATUS "comp are: ${ARG_COMPILE_DEFINITIONS}")
	#message (STATUS "incl are: ${ARG_INCLUDE_DIRECTORIES}")

	add_headers(ARG_SOURCES)
	add_library (${PLUGIN_OBJS} OBJECT ${ARG_SOURCES})

	add_dependencies(${PLUGIN_OBJS} readme_${PLUGIN_SHORT_NAME}.c)

	#message ("generate readme for ${PLUGIN_SHORT_NAME}")
	generate_readme (${PLUGIN_SHORT_NAME})

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY COMPILE_DEFINITIONS
		${ARG_COMPILE_DEFINITIONS}
		"HAVE_KDBCONFIG_H;ELEKTRA_STATIC"
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
		target_link_libraries (${PLUGIN_NAME} ${ARG_LINK_LIBRARIES})
		install (TARGETS ${PLUGIN_NAME} DESTINATION
			lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
		set_property(TARGET ${PLUGIN_NAME}
			APPEND PROPERTY COMPILE_DEFINITIONS
			${ARG_COMPILE_DEFINITIONS}
			"HAVE_KDBCONFIG_H"
			)
		set_property(TARGET ${PLUGIN_NAME}
			APPEND PROPERTY INCLUDE_DIRECTORIES
			${ARG_INCLUDE_DIRECTORIES}
			${CMAKE_CURRENT_BINARY_DIR} #for readme
			)
	endif()

	set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS"
		"$<TARGET_OBJECTS:${PLUGIN_OBJS}>")

	set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES"
		"${ARG_LINK_LIBRARIES}")

endfunction()
