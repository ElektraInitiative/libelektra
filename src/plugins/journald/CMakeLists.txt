if (DEPENDENCY_PHASE)
	find_package (SystemdJournal QUIET)

	if (NOT LIBSYSTEMD_JOURNAL_FOUND)

		remove_plugin (journald "systemd-journal not found")

	endif ()
endif ()

add_plugin (
	journald
	SOURCES journald.h journald.c
	INCLUDE_DIRECTORIES ${LIBSYSTEMD_JOURNAL_INCLUDE_DIR}
	LINK_LIBRARIES ${LIBSYSTEMD_JOURNAL_LIBRARIES} COMPONENT libelektra${SO_VERSION}-journald)
