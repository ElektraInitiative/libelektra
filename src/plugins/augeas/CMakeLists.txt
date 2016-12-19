if (DEPENDENCY_PHASE)
	find_package (Augeas)
	find_package (LibXml2)

	set (LIBAUGEAS_COMPILE_DEFINITIONS LIBAUGEAS_PREFIX="${LIBAUGEAS_PREFIX}")

	if (LIBAUGEAS_FOUND AND LIBXML2_FOUND)
		try_compile (HAS_LIBAUGEAS16
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/augeas/testaugeas.c"
			CMAKE_FLAGS 
				-DINCLUDE_DIRECTORIES:STRING=${LIBAUGEAS_INCLUDE_DIR}\;${LIBXML2_INCLUDE_DIR} 
				-DLINK_LIBRARIES:PATH=${LIBAUGEAS_LIBRARIES}\;${LIBXML2_LIBRARIES}
			)

		if (NOT HAS_LIBAUGEAS16)
			remove_plugin (augeas "symbols aug_text_store and aug_text_retrieve not found. Make sure you have libaugeas 0.16 or higher. This library version is included in Augeas 1.0 or higher")
		endif (NOT HAS_LIBAUGEAS16)
	else (LIBAUGEAS_FOUND AND LIBXML2_FOUND AND)
		remove_plugin (augeas "augeas not found")
	endif (LIBAUGEAS_FOUND AND LIBXML2_FOUND )
endif ()

add_plugin(augeas
	SOURCES
		aug.h
		augeas.c
	ADD_TEST
	INSTALL_TEST_DATA
	INCLUDE_DIRECTORIES
		${LIBAUGEAS_INCLUDE_DIR}
		${LIBXML2_INCLUDE_DIR}
	COMPILE_DEFINITIONS
		"${LIBAUGEAS_COMPILE_DEFINITIONS}"
	LINK_ELEKTRA
		elektra-meta
		elektra-proposal
	LINK_LIBRARIES
		${LIBAUGEAS_LIBRARIES}
		${LIBXML2_LIBRARIES}
	)

if (ADDTESTING_PHASE)
	include (LibAddMacros)
endif ()
