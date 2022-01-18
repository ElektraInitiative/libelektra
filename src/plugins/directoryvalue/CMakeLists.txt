add_plugin (
	directoryvalue CPP ADD_TEST CPP_TEST TEST_README
	SOURCES directoryvalue.hpp directoryvalue.cpp directoryvalue_delegate.hpp directoryvalue_delegate.cpp log.hpp
	LINK_ELEKTRA elektra-ease COMPONENT libelektra${SO_VERSION})
