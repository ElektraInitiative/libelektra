include (LibAddMacros)

add_plugin (
	mathcheck
	SOURCES mathcheck.h mathcheck.c floathelper.h floathelper.c
	TEST_README)
add_plugintest (mathcheck)
