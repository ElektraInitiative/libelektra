try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/fstab/testmntent.c"
		)

include(LibAddMacros)

generate_readme (fstab)

if (HAS_MNTENT)
	set (SOURCES fstab.h fstab.c)
	add_sources (elektra-full ${SOURCES})
	add_headers (SOURCES)

	set (PLUGIN_NAME elektra-fstab)
	add_library (${PLUGIN_NAME} MODULE ${SOURCES})
	target_link_libraries (${PLUGIN_NAME} elektra)

	install(DIRECTORY fstab DESTINATION ${TARGET_TEST_DATA_FOLDER})
	install (TARGETS ${PLUGIN_NAME}
		DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

	add_plugintest (fstab)
else (HAS_MNTENT)
	remove_plugin (fstab "mntent is missing")
endif (HAS_MNTENT)
