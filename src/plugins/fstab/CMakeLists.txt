if (DEPENDENCY_PHASE)
	safe_check_symbol_exists (setmntent "mntent.h" HAS_MNTENT)

	if (NOT HAS_MNTENT)
		remove_plugin (fstab "function mntent is missing")
	endif ()
endif ()

add_plugin (
	fstab
	SOURCES fstab.c fstab.h
	LINK_ELEKTRA elektra-meta
	ADD_TEST INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION}-extra)
