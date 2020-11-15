include (LibAddMacros)

add_plugin (
	keytometa
	SOURCES keytometa.h keytometa.c
	LINK_ELEKTRA elektra-meta
	ADD_TEST)
