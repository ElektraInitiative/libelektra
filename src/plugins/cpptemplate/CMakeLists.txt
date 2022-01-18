include (LibAddMacros)

add_plugin (
	cpptemplate CPP ADD_TEST CPP_TEST TEST_README
	SOURCES cpptemplate.hpp cpptemplate.cpp cpptemplate_delegate.hpp cpptemplate_delegate.cpp COMPONENT
		libelektra${SO_VERSION}-experimental)
