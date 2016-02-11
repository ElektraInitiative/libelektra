include (LibAddPlugin)

# in order to set up OpenSSL development files, please provide:
# - OPENSSL_INCLUDE_DIR
# - OPENSSL_LIBRARIES
#
# in order to set up libgcrypt development files, please provide:
# - LIBGCRYPT_INCLUDE_DIR
# - LIBGCRYPT_LIBRARIES

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
# check if library detection is required
#
if (DEFINED OPENSSL_INCLUDE_DIR AND DEFINED OPENSSL_LIBRARIES)
	set (OPENSSL_MANUAL_CONFIG on)
	set (OPENSSL_FOUND on)
else ()
	set (OPENSSL_MANUAL_CONFIG off)
endif ()

if (DEFINED LIBGCRYPT_INCLUDE_DIR AND DEFINED LIBGCRYPT_LIBRARIES)
	set (LIBGCRYPT_MANUAL_CONFIG on)
	set (LIBGCRYPT_FOUND on)
else ()
	set (LIBGCRYPT_MANUAL_CONFIG off)
endif ()

#
# Compile Variants
#
foreach (plugin ${PLUGINS})

	# Compile Variant: OpenSSL
	if (${plugin} MATCHES "crypto_openssl")

		if (NOT OPENSSL_MANUAL_CONFIG)
			include (FindPkgConfig)
			pkg_search_module (OPENSSL openssl)
		endif ()

		if ((NOT OPENSSL_FOUND) AND (NOT OPENSSL_MANUAL_CONFIG))
			remove_plugin ( ${plugin} "OpenSSL development files not found")
		else  ()

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
			else ()
				remove_plugin ( ${plugin} "OpenSSL compilation test failed")
				set (OPENSSL_FOUND off)
			endif (HAS_OPENSSL_4SURE)
		endif ((NOT OPENSSL_FOUND) AND (NOT OPENSSL_MANUAL_CONFIG))

	endif (${plugin} MATCHES "crypto_openssl")

	# Compile Variant: libgcrypt
	if (${plugin} MATCHES "crypto_gcrypt")

		if (NOT LIBGCRYPT_MANUAL_CONFIG)
			find_package (LibGcrypt)
		endif ()

		if ((NOT LIBGCRYPT_FOUND) AND (NOT LIBGCRYPT_MANUAL_CONFIG))
			remove_plugin ( ${plugin} "libgcrypt development files not found")
		else ()

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
			else ()
				remove_plugin ( ${plugin} "libgcrypt compilation test failed")
				set (LIBGCRYPT_FOUND off)
			endif (HAS_GCRYPT_4SURE)
		endif ((NOT LIBGCRYPT_FOUND) AND (NOT LIBGCRYPT_MANUAL_CONFIG))

	endif (${plugin} MATCHES "crypto_gcrypt")

endforeach (plugin)

unset (OPENSSL_MANUAL_CONFIG)
unset (LIBGCRYPT_MANUAL_CONFIG)