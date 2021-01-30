include (LibAddMacros)

add_plugin (
	mathcheck
	SOURCES mathcheck.h mathcheck.c floathelper.h floathelper.c
	TEST_README COMPONENT libelektra${SO_VERSION}-extra)
add_plugintest (mathcheck)
