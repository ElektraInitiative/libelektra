
function (set_additional_compile_definitions shortname)
	if (NOT "${ARG_COMPILE_DEFINITIONS}" MATCHES "ELEKTRA_MODULE_NAME")
		list (APPEND ADDITIONAL_COMPILE_DEFINITIONS_PARTS
			"ELEKTRA_MODULE_NAME=${shortname}")
	endif ()

	set (ADDITIONAL_COMPILE_DEFINITIONS "${ADDITIONAL_COMPILE_DEFINITIONS_PARTS}" PARENT_SCOPE)
	unset (ADDITIONAL_COMPILE_DEFINITIONS_PARTS)
endfunction (set_additional_compile_definitions)

function (add_lib name)
	cmake_parse_arguments (ARG
			       "CPP" # optional keywords
			       "" # one value keywords
			       "SOURCES;LINK_LIBRARIES;LINK_ELEKTRA" # multi value keywords
			       ${ARGN})
	add_headers (ARG_SOURCES)
	if (ARG_CPP)
		add_cppheaders (ARG_SOURCES)
	endif (ARG_CPP)

	set_additional_compile_definitions (${name})

	if (BUILD_SHARED)
		add_library (elektra-${name} SHARED ${ARG_SOURCES})
		add_dependencies (elektra-${name} kdberrors_generated elektra_error_codes_generated ${ARG_LINK_ELEKTRA})

		set_property (TARGET elektra-${name}
			APPEND
			PROPERTY COMPILE_DEFINITIONS
			${ADDITIONAL_COMPILE_DEFINITIONS}
			)

		target_link_libraries (elektra-${name} elektra-core ${ARG_LINK_ELEKTRA})
	endif (BUILD_SHARED)

	set_property (GLOBAL
		      APPEND
		      PROPERTY "elektra-full_SRCS"
			       ${ARG_SOURCES})

	set_property (GLOBAL
		      APPEND
		      PROPERTY "elektra-extension_LIBRARIES"
			       elektra-${name})

	if (BUILD_SHARED)
		set_property (TARGET elektra-${name}
			APPEND
			PROPERTY COMPILE_DEFINITIONS
			${ADDITIONAL_COMPILE_DEFINITIONS}
			)

		target_link_libraries (elektra-${name} ${ARG_LINK_LIBRARIES})

		install (TARGETS elektra-${name} DESTINATION lib${LIB_SUFFIX} EXPORT ElektraTargetsLibelektra)
	endif (BUILD_SHARED)

	unset (ADDITIONAL_COMPILE_DEFINITIONS)
endfunction ()
