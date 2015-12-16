# set default value to CRYPTO_API
if (NOT DEFINED CRYPTO_API OR CRYPTO_API STREQUAL "")
	set (CRYPTO_API "gcrypt")
	message ("Trying to use gcrypt as default crypto API")
endif (NOT DEFINED CRYPTO_API OR CRYPTO_API STREQUAL "")

set (ELEKTRA_CRYPTO_API_GCRYPT FALSE)
set (ELEKTRA_CRYPTO_API_OPENSSL FALSE)

# check if crypto API is available
if (CRYPTO_API STREQUAL "gcrypt")
	find_package (LibGcrypt)
	if (NOT LIBGCRYPT_FOUND)
		remove_plugin (crypto "libgcrypt development files not found")
		return ()
	endif (NOT LIBGCRYPT_FOUND)
	set (ELEKTRA_CRYPTO_API_GCRYPT TRUE)
endif (CRYPTO_API STREQUAL "gcrypt")

if (CRYPTO_API STREQUAL "openssl")
	find_package (OpenSSL)
	if (NOT OPENSSL_FOUND)
		remove_plugin (crypto "OpenSSL development files not found")
		return ()
	endif (NOT OPENSSL_FOUND)
	set (ELEKTRA_CRYPTO_API_OPENSSL TRUE)
endif (CRYPTO_API STREQUAL "openssl")

if (NOT DEFINED ELEKTRA_CRYPTO_API_GCRYPT AND NOT DEFINED ELEKTRA_CRYPTO_API_OPENSSL)
	message( FATAL_ERROR "You provided an invalid crypto API: ${CRYPTO_API}" )
endif (NOT DEFINED ELEKTRA_CRYPTO_API_GCRYPT AND NOT DEFINED ELEKTRA_CRYPTO_API_OPENSSL)

# configure build process
configure_file (
	"${CMAKE_CURRENT_SOURCE_DIR}/crypto_internal.h.in"
	"${CMAKE_CURRENT_BINARY_DIR}/crypto_internal.h"
)

install (FILES
	"${CMAKE_CURRENT_BINARY_DIR}/crypto_internal.h"
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
)

include_directories("${CMAKE_CURRENT_BINARY_DIR}")
include(LibAddPlugin)

if (ELEKTRA_CRYPTO_API_GCRYPT)
	add_plugin (crypto
		SOURCES 
			gcrypt_operations.h
			gcrypt_operations.c
			crypto.h
			crypto.c
		INCLUDE_DIRECTORIES
			${LIBGCRYPT_INCLUDE_DIR}
		LINK_LIBRARIES
			${LIBGCRYPT_LIBRARIES}
		)
	message ("crypto setup seems to be good with gcrypt")
endif (ELEKTRA_CRYPTO_API_GCRYPT)

if (ELEKTRA_CRYPTO_API_OPENSSL)
	add_plugin (crypto
		SOURCES 
			openssl_operations.h
			openssl_operations.c
			crypto.h
			crypto.c
		INCLUDE_DIRECTORIES
			${OPENSSL_INCLUDE_DIR}
		LINK_LIBRARIES
			${OPENSSL_LIBRARIES}
		)
	message ("crypto setup seems to be good with OpenSSL")
endif (ELEKTRA_CRYPTO_API_OPENSSL)

add_plugintest (crypto)
