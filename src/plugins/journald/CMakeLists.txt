find_package(SystemdJournal)

if (LIBSYSTEMD_JOURNAL_FOUND)

	include (LibAddMacros)

	add_plugin(journald
		SOURCES
			journald.h
			journald.c
		INCLUDE_DIRECTORIES
			${LIBSYSTEMD_JOURNAL_INCLUDE_DIR}
		LINK_LIBRARIES
			${LIBSYSTEMD_JOURNAL_LIBRARIES}
		)

	#add_plugintest (journald)

else (LIBSYSTEMD_JOURNAL_FOUND)

	remove_plugin (journald "systemd-journal not found")

endif (LIBSYSTEMD_JOURNAL_FOUND)

