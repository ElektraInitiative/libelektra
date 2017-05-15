include (LibAddMacros)

add_plugin (multifile
	SOURCES
		multifile.h
		multifile.c
	LINK_ELEKTRA
		elektra-kdb
	)
