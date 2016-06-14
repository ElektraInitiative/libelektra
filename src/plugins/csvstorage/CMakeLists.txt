include (LibAddMacros)

add_plugin (csvstorage
	SOURCES
		csvstorage.h
		csvstorage.c
	LINK_ELEKTRA
		elektra-ease
	ADD_TEST
	INSTALL_TEST_DATA
	)
