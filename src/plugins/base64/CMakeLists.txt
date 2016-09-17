include (LibAddPlugin)

add_plugin (base64
	SOURCES
		base64.h
		base64.c
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"base64\"
)

if (ADDTESTING_PHASE)
	add_plugintest (base64)
endif ()
