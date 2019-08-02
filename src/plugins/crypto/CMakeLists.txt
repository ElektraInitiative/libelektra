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
	find_package (Libgcrypt QUIET)
	if (NOT LIBGCRYPT_FOUND)
		remove_plugin (${plugin} "libgcrypt development files not found")
	endif ()

	# Plugin variant: crypto_botan
	set (plugin crypto_botan)
	find_package (LibBotan)
	if (NOT Botan_FOUND)
		remove_plugin (${plugin} "botan development files not found")
	endif ()
	if (ENABLE_ASAN AND (CMAKE_C_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU"))
		remove_plugin (${plugin} "ENABLE_ASAN + GNU not supported by botan")
	endif ()

	# clean up for dependency phase
	unset (plugin)
endif ()

set (CRYPTO_COMMON_FILES
     helper.h
     helper.c
     gpg.h
     gpg.c
     crypto.h
     crypto.c)

#
# Compile Variant: libgcrypt
#
add_plugin (crypto_gcrypt
	    SOURCES gcrypt_operations.h gcrypt_operations.c ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${Libgcrypt_INCLUDE_DIRS}
	    LINK_LIBRARIES ${Libgcrypt_LIBRARIES}
	    LINK_ELEKTRA elektra-invoke
	    COMPILE_DEFINITIONS ELEKTRA_VARIANT=gcrypt ELEKTRA_CRYPTO_API_GCRYPT)

#
# Compile Variant: OpenSSL
#
add_plugin (crypto_openssl
	    SOURCES openssl_operations.h openssl_operations.c ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${OPENSSL_INCLUDE_DIR}
	    LINK_LIBRARIES ${OPENSSL_LIBRARIES}
	    LINK_ELEKTRA elektra-invoke
	    COMPILE_DEFINITIONS ELEKTRA_VARIANT=openssl ELEKTRA_CRYPTO_API_OPENSSL)

#
# Compile Variant: Botan
#
add_plugin (crypto_botan
	    SOURCES botan_operations.h botan_operations.cpp ${CRYPTO_COMMON_FILES}
	    INCLUDE_DIRECTORIES ${Botan_INCLUDE_DIRS}
	    LINK_LIBRARIES ${Botan_LIBRARIES}
	    LINK_ELEKTRA elektra-invoke
	    COMPILE_DEFINITIONS ELEKTRA_VARIANT=botan ELEKTRA_CRYPTO_API_BOTAN)

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
