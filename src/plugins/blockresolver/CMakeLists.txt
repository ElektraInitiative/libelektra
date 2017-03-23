include (LibAddMacros)

add_plugin (blockresolver
	SOURCES
		blockresolver.h
		blockresolver.c
		resolve.h
		resolve.c
	ADD_TEST
	INSTALL_TEST_DATA
	)
