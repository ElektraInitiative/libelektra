function(add_lib name)
	cmake_parse_arguments (ARG
		"CPP" # optional keywords
		"" # one value keywords
		"SOURCES;LINK_LIBRARIES;LINK_ELEKTRA" # multi value keywords
		${ARGN}
	)

	add_headers(ARG_SOURCES)
	if (ARG_CPP)
		add_cppheaders(ARG_SOURCES)
	endif (ARG_CPP)

	if (BUILD_SHARED)
		add_library (elektra-${name} SHARED ${ARG_SOURCES})
		add_dependencies (elektra-${name} kdberrors_generated)

		target_link_libraries (elektra-${name} elektra-core ${ARG_LINK_ELEKTRA})
	endif (BUILD_SHARED)

	set_property (GLOBAL APPEND PROPERTY "elektra-full_SRCS"
		${ARG_SOURCES}
		)

	set_property (GLOBAL APPEND PROPERTY "elektra-extension_LIBRARIES"
		elektra-${name}
		)

	if (BUILD_SHARED)
		target_link_libraries (elektra-${name}
			${ARG_LINK_LIBRARIES})

		install (TARGETS elektra-${name} DESTINATION lib${LIB_SUFFIX} EXPORT ElektraTargetsLibelektra)
	endif (BUILD_SHARED)

endfunction()
