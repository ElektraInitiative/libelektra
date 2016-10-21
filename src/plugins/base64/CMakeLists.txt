include (LibAddPlugin)

add_plugin (base64
	SOURCES
		base64_functions.h
		base64_functions.c
		base64.h
		base64.c
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"base64\"
		ELEKTRA_PLUGIN_NAME_C=base64
)

if (ADDTESTING_PHASE)
	add_plugintest (base64)
endif ()
