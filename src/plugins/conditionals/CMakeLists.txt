include (LibAddMacros)

# ——————————————————————————————————————————————————————————
# TODO: Remove this after we enabled `-Wconversion` globally
set_source_files_properties (conditionals.c conditionals.h testmod_conditionals.c PROPERTIES COMPILE_FLAGS "-Wconversion")
# ——————————————————————————————————————————————————————————

add_plugin (
	conditionals
	SOURCES conditionals.h conditionals.c
	LINK_ELEKTRA elektra-meta
	ADD_TEST TEST_README
	TEST_REQUIRED_PLUGINS dump ini ni COMPONENT libelektra${SO_VERSION}-extra)
