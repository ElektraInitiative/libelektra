include (LibAddMacros)

add_plugin (hosts
	SOURCES
		hosts.h
		hosts-get.c
		hosts-set.c
		keymetaformatting.h
		keymetaformatting.c
	LINK_ELEKTRA
		elektra-ease
		elektra-proposal
		elektra-meta
	ADD_TEST
	INSTALL_TEST_DATA
	)
