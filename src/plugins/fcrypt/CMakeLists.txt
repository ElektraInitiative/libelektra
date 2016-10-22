include (LibAddPlugin)

add_plugin (fcrypt
	SOURCES
		../base64/base64_functions.h
		../base64/base64_functions.c
		../crypto/helper.h
		../crypto/helper.c
		../crypto/gpg.h
		../crypto/gpg.c
		fcrypt.h
		fcrypt.c
	INCLUDE_DIRECTORIES
		${CMAKE_SOURCE_DIR}/src/plugins/crypto/
		${CMAKE_SOURCE_DIR}/src/plugins/base64/
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"fcrypt\"
		ELEKTRA_PLUGIN_NAME_C=fcrypt
)

if (ADDTESTING_PHASE)
	add_plugintest (fcrypt)
endif ()
