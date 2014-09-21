try_compile (HAS_MNTENT
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/fstab/testmntent.c"
		)

include(LibAddMacros)

generate_readme (fstab)
add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR})

if (HAS_MNTENT)
	add_plugin(fstab
		SOURCES
			fstab.c
			fstab.h
		)

	add_plugintest (fstab)
else (HAS_MNTENT)
	remove_plugin (fstab "mntent is missing")
endif (HAS_MNTENT)
