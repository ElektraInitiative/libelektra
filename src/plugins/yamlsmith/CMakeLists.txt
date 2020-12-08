add_plugin (
	yamlsmith CPP
	SOURCES yamlsmith.hpp yamlsmith.cpp
	LINK_ELEKTRA elektra-ease
	ADD_TEST TEST_README COMPONENT libelektra4-extra)
