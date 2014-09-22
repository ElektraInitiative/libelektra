find_package(SystemdJournal REQUIRED)

if(LIBSYSTEMD_JOURNAL_FOUND)

	include (LibAddMacros)

	add_plugin(journald
		SOURCES
			journald.h
			journald.c
		)

	#add_plugintest (journald)

else (LIBSYSTEMD_JOURNAL_FOUND)

	remove_plugin (journald "systemd-journal not found")

endif (LIBSYSTEMD_JOURNAL_FOUND)

