include (LibAddPlugin)

if (DEPENDENCY_PHASE)

	# Plugin variant: crypto_openssl
	set (plugin crypto_openssl)
	find_package (LibOpenSSL)
	if (NOT OPENSSL_FOUND)
		remove_plugin (${plugin} "OpenSSL development files not found")
	endif ()

	# Plugin variant: crypto_gcrypt
	set (plugin crypto_gcrypt)
	find_package (LibGcrypt QUIET)
	if (NOT LIBGCRYPT_FOUND)
		remove_plugin (${plugin} "libgcrypt development files not found")
	endif ()

	# Plugin variant: crypto_botan
	set (plugin crypto_botan)
	find_package (LibBotan)
	if (NOT BOTAN_FOUND)
		remove_plugin (${plugin} "botan development files not found")
	endif ()

	# clean up for dependency phase
	unset (plugin)

	# add base64 directory to include path
	if (NOT "${OPENSSL_INCLUDE_DIR}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (OPENSSL_INCLUDE_DIR
		     ${OPENSSL_INCLUDE_DIR}
		     "${CMAKE_SOURCE_DIR}/src/plugins/base64/"
		     CACHE STRING
			   "OpenSSL include path"
		     FORCE)
	endif ()

	if (NOT "${LIBGCRYPT_INCLUDE_DIR}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (LIBGCRYPT_INCLUDE_DIR
		     ${LIBGCRYPT_INCLUDE_DIR}
		     "${CMAKE_SOURCE_DIR}/src/plugins/base64/"
		     CACHE STRING
			   "libgcrypt include path"
		     FORCE)
	endif ()

	if (NOT "${BOTAN_INCLUDE_DIRS}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (BOTAN_INCLUDE_DIRS
		     ${BOTAN_INCLUDE_DIRS}
		     "${CMAKE_SOURCE_DIR}/src/plugins/base64/"
		     CACHE STRING
			   "Botan include path"
		     FORCE)
	endif ()
endif ()

set (CRYPTO_COMMON_FILES ../base64/base64_functions.h ../base64/base64_functions.c helper.h helper.c gpg.h gpg.c crypto.h crypto.c)

#
# Compile Variant: OpenSSL
#
add_plugin (crypto_gcrypt
	    SOURCES gcrypt_operations.h
		    gcrypt_operations.c
		    ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${LIBGCRYPT_INCLUDE_DIR}
	    LINK_LIBRARIES ${LIBGCRYPT_LIBRARIES}
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"crypto_gcrypt\"
				ELEKTRA_PLUGIN_NAME_C=crypto
				ELEKTRA_VARIANT=gcrypt
				ELEKTRA_CRYPTO_API_GCRYPT)

#
# Compile Variant: libgcrypt
#
add_plugin (crypto_openssl
	    SOURCES openssl_operations.h
		    openssl_operations.c
		    ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${OPENSSL_INCLUDE_DIR}
	    LINK_LIBRARIES ${OPENSSL_LIBRARIES}
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"crypto_openssl\"
				ELEKTRA_PLUGIN_NAME_C=crypto
				ELEKTRA_VARIANT=openssl
				ELEKTRA_CRYPTO_API_OPENSSL)

#
# Compile Variant: Botan
#
add_plugin (crypto_botan
	    SOURCES botan_operations.h
		    botan_operations.cpp
		    ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${BOTAN_INCLUDE_DIRS}
	    LINK_LIBRARIES ${BOTAN_LIBRARIES}
	    COMPILE_DEFINITIONS ELEKTRA_PLUGIN_NAME=\"crypto_botan\"
				ELEKTRA_PLUGIN_NAME_C=crypto
				ELEKTRA_VARIANT=botan
				ELEKTRA_CRYPTO_API_BOTAN)

if (ADDTESTING_PHASE)
	if (HAS_OPENSSL_4SURE)
		add_plugintest (crypto_openssl LINK_PLUGIN "crypto_openssl")
	endif ()

	if (HAS_GCRYPT_4SURE)
		add_plugintest (crypto_gcrypt MEMLEAK LINK_PLUGIN "crypto_gcrypt")
	endif ()

	if (HAS_BOTAN_4SURE)
		add_plugintest (crypto_botan LINK_PLUGIN "crypto_botan")
	endif ()
endif ()

unset (CRYPTO_COMMON_FILES)
