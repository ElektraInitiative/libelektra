add_plugin (
	kconfig CPP ADD_TEST CPP_TEST INSTALL_TEST_DATA TEST_README
	SOURCES kconfig.hpp
		kconfig.cpp
		kconfig_delegate.hpp
		kconfig_delegate.cpp
		kconfig_parser.cpp
		kconfig_parser_exception.cpp
		kconfig_serializer.cpp
		file_utility.cpp
		COMPONENT
		libelektra${SO_VERSION})
