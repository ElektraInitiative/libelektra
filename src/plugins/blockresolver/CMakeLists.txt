include (LibAddMacros)

add_plugin (blockresolver
	SOURCES
		blockresolver.h
		blockresolver.c
	LINK_ELEKTRA
		elektra-proposal
	ADD_TEST
	INSTALL_TEST_DATA
	)
