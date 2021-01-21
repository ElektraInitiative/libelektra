include (LibAddMacros)

add_plugin (
	hosts
	SOURCES hosts.h hosts-get.c hosts-set.c keymetaformatting.h keymetaformatting.c
	LINK_ELEKTRA elektra-ease elektra-meta
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS network
	INSTALL_TEST_DATA COMPONENT libelektra${SO_VERSION})
