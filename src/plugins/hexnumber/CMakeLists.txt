include (LibAddMacros)

add_plugin (
	hexnumber
	SOURCES hexnumber.h hexnumber.c
	LINK_ELEKTRA elektra-ease
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-extra)
