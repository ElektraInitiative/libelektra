include (LibAddPlugin)

add_plugin (base666
	SOURCES
		../base64/base64_functions.h
		../base64/base64_functions.c
		base666.h
		base666.c
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME_C=base666
)
