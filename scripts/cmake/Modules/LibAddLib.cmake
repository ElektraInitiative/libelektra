function (set_additional_compile_definitions shortname)
	if (NOT "${ARG_COMPILE_DEFINITIONS}" MATCHES "ELEKTRA_MODULE_NAME")
		list (APPEND ADDITIONAL_COMPILE_DEFINITIONS_PARTS "ELEKTRA_MODULE_NAME=${shortname}")
	endif ()

	set (
		ADDITIONAL_COMPILE_DEFINITIONS
		"${ADDITIONAL_COMPILE_DEFINITIONS_PARTS}"
		PARENT_SCOPE)
	unset (ADDITIONAL_COMPILE_DEFINITIONS_PARTS)
endfunction (set_additional_compile_definitions)

function (add_lib name)
	cmake_parse_arguments (
		ARG
		"CPP" # optional keywords
		"COMPONENT" # one value keywords
		"SOURCES;LINK_LIBRARIES;LINK_ELEKTRA;INCLUDE_DIRECTORIES;INCLUDE_SYSTEM_DIRECTORIES;COMPILE_DEFINITIONS" # multi value
		# keywords
		${ARGN})

	if (ARG_COMPONENT)
		set (HAS_COMPONENT ${ARG_COMPONENT})
	else ()
		set (HAS_COMPONENT "${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME}")
	endif ()

	add_library (elektra-${name}-objects OBJECT ${ARG_SOURCES})
	add_dependencies (elektra-${name}-objects generate_version_script)
	target_include_directories (elektra-${name}-objects PUBLIC ${ARG_INCLUDE_DIRECTORIES})
	target_include_directories (elektra-${name}-objects SYSTEM PUBLIC ${ARG_INCLUDE_SYSTEM_DIRECTORIES})

	set_property (TARGET elektra-${name}-objects PROPERTY POSITION_INDEPENDENT_CODE ON)

	set_additional_compile_definitions (${name})

	set_property (
		TARGET elektra-${name}-objects
		APPEND
		PROPERTY COMPILE_DEFINITIONS "${ARG_COMPILE_DEFINITIONS};${ADDITIONAL_COMPILE_DEFINITIONS}")

	if (BUILD_SHARED)
		add_library (elektra-${name} SHARED $<TARGET_OBJECTS:elektra-${name}-objects>)

		target_link_libraries (elektra-${name} ${ARG_LINK_ELEKTRA})
		if (NOT ${name} STREQUAL "core")
			target_link_libraries (elektra-${name} elektra-core)
		endif ()
	endif (BUILD_SHARED)

	if (BUILD_SHARED)
		target_link_libraries (elektra-${name} ${ARG_LINK_LIBRARIES})

		if (${LD_ACCEPTS_VERSION_SCRIPT})
			set_target_properties (elektra-${name} PROPERTIES LINK_FLAGS
									  "-Wl,--version-script='${CMAKE_BINARY_DIR}/src/libs/symbols.map'")
		endif ()

		install (
			TARGETS elektra-${name}
			DESTINATION lib${LIB_SUFFIX}
			COMPONENT "${HAS_COMPONENT}"
			EXPORT ElektraTargetsLibelektra)
	endif (BUILD_SHARED)

	if (ADDED_LIBRARIES)
		set (TMP "${ADDED_LIBRARIES};${name}")
		list (SORT TMP)
		list (REMOVE_DUPLICATES TMP)
		set (
			ADDED_LIBRARIES
			"${TMP}"
			CACHE STRING "${ADDED_LIBRARIES_DOC}" FORCE)
	else ()
		set (
			ADDED_LIBRARIES
			"${name}"
			CACHE STRING "${ADDED_LIBRARIES_DOC}" FORCE)
	endif ()

	set_property (GLOBAL APPEND PROPERTY "ELEKTRA_LINK_LIBRARIES" "${ARG_LINK_LIBRARIES}")

endfunction ()
