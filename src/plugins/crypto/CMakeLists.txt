include (LibAddPlugin)

set (ELEKTRA_CRYPTO_API_GCRYPT FALSE)
set (ELEKTRA_CRYPTO_API_OPENSSL FALSE)

#
# base plugin (that does actually nothing on its own)
#
add_plugin (crypto
	SOURCES
		crypto.h
		crypto.c
	INCLUDE_DIRECTORIES
		${OPENSSL_INCLUDE_DIR}
	LINK_LIBRARIES
		${OPENSSL_LIBRARIES}
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"crypto\"
)

#add_plugintest (crypto)

#
# Compile Variants
#
foreach (plugin ${PLUGINS})

	# Compile Variant: OpenSSL
	if (${plugin} MATCHES "crypto_openssl")

		include (FindPkgConfig)
		pkg_search_module (OPENSSL openssl)
		if (NOT OPENSSL_FOUND)
			message ( FATAL_ERROR "OpenSSL development files not found")
			return ()
		endif (NOT OPENSSL_FOUND)

		add_plugin (${plugin}
			SOURCES
				openssl_operations.h
				openssl_operations.c
				crypto.h
				crypto.c
			INCLUDE_DIRECTORIES
				${OPENSSL_INCLUDE_DIR}
			LINK_LIBRARIES
				${OPENSSL_LIBRARIES}
			COMPILE_DEFINITIONS
				ELEKTRA_PLUGIN_NAME=\"${plugin}\"
				ELEKTRA_CRYPTO_API_OPENSSL
		)

	endif (${plugin} MATCHES "crypto_openssl")

	# Compile Variant: libgcrypt
	if (${plugin} MATCHES "crypto_gcrypt")

		find_package (LibGcrypt)
		if (NOT LIBGCRYPT_FOUND)
			message ( FATAL_ERROR "libgcrypt development files not found")
			return ()
		endif (NOT LIBGCRYPT_FOUND)

		add_plugin (${plugin}
			SOURCES
				gcrypt_operations.h
				gcrypt_operations.c
				crypto.h
				crypto.c
			INCLUDE_DIRECTORIES
				${LIBGCRYPT_INCLUDE_DIR}
			LINK_LIBRARIES
				${LIBGCRYPT_LIBRARIES}
			COMPILE_DEFINITIONS
				ELEKTRA_PLUGIN_NAME=\"${plugin}\"
				ELEKTRA_CRYPTO_API_GCRYPT
		)

	endif (${plugin} MATCHES "crypto_gcrypt")

endforeach (plugin)
