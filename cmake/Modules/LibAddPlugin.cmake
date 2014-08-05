include(LibParseArguments)
include(LibAddMacros)

function(add_plugin PLUGIN_SHORT_NAME)

	parse_arguments(ARG
		"SOURCES;LINK_LIBRARIES;COMPILE_DEFINITIONS;INCLUDE_DIRECTORIES"
		"" # no option
		${ARGN}
		)


	set (PLUGIN_NAME elektra-${PLUGIN_SHORT_NAME})
	set (PLUGIN_OBJS ${PLUGIN_NAME}-objects)

	message (STATUS "name: ${PLUGIN_NAME}")
	message (STATUS "srcs are: ${ARG_SOURCES}")
	message (STATUS "deps are: ${ARG_LINK_LIBRARIES}")
	message (STATUS "comp are: ${ARG_COMPILE_DEFINITIONS}")
	message (STATUS "incl are: ${ARG_INCLUDE_DIRECTORIES}")

	add_library (${PLUGIN_OBJS} OBJECT ${ARG_SOURCES})

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY COMPILE_DEFINITIONS
		${ARG_COMPILE_DEFINITIONS})

	set_property(TARGET ${PLUGIN_OBJS}
		APPEND PROPERTY INCLUDE_DIRECTORIES
		${ARG_INCLUDE_DIRECTORIES})

	#set_property(TARGET ${PLUGIN_OBJS}
	#	APPEND PROPERTY COMPILE_FLAGS
	#	-fPIC) # needed for shared libraries

	set_property(TARGET ${PLUGIN_OBJS}
		PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)


	if (BUILD_SHARED)
		add_library (${PLUGIN_NAME} MODULE $<TARGET_OBJECTS:${PLUGIN_OBJS}>)
		target_link_libraries (${PLUGIN_NAME} ${ARG_LINK_LIBRARIES})
		install (TARGETS ${PLUGIN_NAME} DESTINATION
			lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})
	endif()

	add_sources(elektra-full $<TARGET_OBJECTS:${PLUGIN_OBJS}>)

endfunction()
