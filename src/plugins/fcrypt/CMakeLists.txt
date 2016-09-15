include (LibAddPlugin)

add_plugin (fcrypt
	SOURCES
		../crypto/helper.h
		../crypto/helper.c
		../crypto/gpg.h
		../crypto/gpg.c
		fcrypt.h
		fcrypt.c
	INCLUDE_DIRECTORIES
		${CMAKE_SOURCE_DIR}/src/plugins/crypto/
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"fcrypt\"
)

if (ADDTESTING_PHASE)
	add_plugintest (fcrypt)
endif ()
