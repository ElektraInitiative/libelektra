include (LibAddMacros)

add_plugin (doc
	SOURCES
		doc.h doc.c
	LINK_ELEKTRA
		elektra-kdb
)

