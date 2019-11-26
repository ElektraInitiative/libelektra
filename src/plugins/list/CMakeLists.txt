include (LibAddMacros)

add_plugin (
	list
	SOURCES list.h list.c
	LINK_ELEKTRA elektra-kdb elektra-invoke elektra-ease elektra-proposal
	ADD_TEST)
