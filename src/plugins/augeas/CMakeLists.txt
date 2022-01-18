if (DEPENDENCY_PHASE)
	find_package (Augeas QUIET)
	find_package (LibXml2 QUIET)

	set (LIBAUGEAS_COMPILE_DEFINITIONS LIBAUGEAS_PREFIX="${LIBAUGEAS_PREFIX}")

	if (LIBAUGEAS_FOUND)
		if (LIBXML2_FOUND)
			try_compile (
				HAS_LIBAUGEAS16 "${CMAKE_BINARY_DIR}"
				"${PROJECT_SOURCE_DIR}/src/plugins/augeas/testaugeas.c"
				CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${LIBAUGEAS_INCLUDE_DIR}\;${LIBXML2_INCLUDE_DIR}
					    -DLINK_LIBRARIES:PATH=${LIBAUGEAS_LIBRARIES}\;${LIBXML2_LIBRARIES})

			if (NOT HAS_LIBAUGEAS16)
				remove_plugin (
					augeas "symbols aug_text_store and aug_text_retrieve not found. "
					"Make sure you have libaugeas 0.16 or higher. "
					"This library version is included in Augeas 1.0 or higher")
			endif (NOT HAS_LIBAUGEAS16)
		else (LIBXML2_FOUND)
			remove_plugin (augeas "libxml2 not found")
		endif ()
	else (LIBAUGEAS_FOUND)
		remove_plugin (augeas "libaugeas not found (libaugeas-dev >= 0.16 needed)")
	endif ()

	set (AUGEAS_PLUGIN_INCLUDE_DIRS ${LIBAUGEAS_INCLUDE_DIR} ${LIBXML2_INCLUDE_DIR})
	set (AUGEAS_PLUGIN_LIBRARIES ${LIBAUGEAS_LIBRARIES} ${LIBXML2_LIBRARIES})
endif ()

# ——————————————————————————————————————————————————————————
# TODO: Remove this after we enabled `-Wconversion` globally
set_source_files_properties (aug.h augeas.c PROPERTIES COMPILE_FLAGS "-Wconversion")
# ——————————————————————————————————————————————————————————

# ~~~
# The unit test for the plugin fails on an ASAN enabled macOS build, if we do not disable container overflow detection.
# Reference: https://travis-ci.org/sanssecours/elektra/jobs/418554628
# ~~~
add_plugin (
	augeas
	SOURCES aug.h augeas.c
	ADD_TEST INSTALL_TEST_DATA
	INCLUDE_DIRECTORIES ${AUGEAS_PLUGIN_INCLUDE_DIRS}
	COMPILE_DEFINITIONS "${LIBAUGEAS_COMPILE_DEFINITIONS}"
	LINK_ELEKTRA elektra-meta
	LINK_LIBRARIES ${AUGEAS_PLUGIN_LIBRARIES}
	TEST_ENVIRONMENT "ASAN_OPTIONS=detect_container_overflow=0" COMPONENT libelektra${SO_VERSION}-augeas)
