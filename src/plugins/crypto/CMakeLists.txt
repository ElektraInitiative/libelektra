include (LibAddPlugin)

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

add_plugintest (crypto)

#
# Compile Variants
#
foreach (plugin ${PLUGINS})

	# Compile Variant: OpenSSL
	if (${plugin} MATCHES "crypto_openssl")

		include (FindPkgConfig)
		pkg_search_module (OPENSSL openssl)
		if (NOT OPENSSL_FOUND)
			remove_plugin ( ${plugin} "OpenSSL development files not found")
		else  (NOT OPENSSL_FOUND)

			try_compile (HAS_OPENSSL_4SURE
				"${CMAKE_BINARY_DIR}"
				"${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_openssl.c"
				CMAKE_FLAGS
					-DINCLUDE_DIRECTORIES:STRING=${OPENSSL_INCLUDE_DIR}
					-DLINK_LIBRARIES:PATH=${OPENSSL_LIBRARIES}
			)

			if (HAS_OPENSSL_4SURE)
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
						ELEKTRA_VARIANT=openssl
						ELEKTRA_CRYPTO_API_OPENSSL
				)
			endif (HAS_OPENSSL_4SURE)
		endif (NOT OPENSSL_FOUND)

	endif (${plugin} MATCHES "crypto_openssl")

	# Compile Variant: libgcrypt
	if (${plugin} MATCHES "crypto_gcrypt")

		find_package (LibGcrypt)
		if (NOT LIBGCRYPT_FOUND)
			remove_plugin ( ${plugin} "libgcrypt development files not found")
		else (NOT LIBGCRYPT_FOUND)

			try_compile (HAS_GCRYPT_4SURE
				"${CMAKE_BINARY_DIR}"
				"${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_gcrypt.c"
				CMAKE_FLAGS
					-DINCLUDE_DIRECTORIES:STRING=${LIBGCRYPT_INCLUDE_DIR}
					-DLINK_LIBRARIES:PATH=${LIBGCRYPT_LIBRARIES}
			)

			if (HAS_GCRYPT_4SURE)
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
						ELEKTRA_VARIANT=gcrypt
						ELEKTRA_CRYPTO_API_GCRYPT
				)
			endif (HAS_GCRYPT_4SURE)
		endif (NOT LIBGCRYPT_FOUND)

	endif (${plugin} MATCHES "crypto_gcrypt")

endforeach (plugin)
