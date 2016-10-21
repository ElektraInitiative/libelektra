include (LibAddPlugin)

#
# NOTE
#
# in order to set up OpenSSL development files manually, please provide:
# - OPENSSL_INCLUDE_DIR
# - OPENSSL_LIBRARIES
#
# in order to set up libgcrypt development files manually, please provide:
# - LIBGCRYPT_INCLUDE_DIR
# - LIBGCRYPT_LIBRARIES
#
# in order to set up Botan development files manually, please provide:
# - BOTAN_INCLUDE_DIRS
# - BOTAN_LIBRARIES
#
if (DEPENDENCY_PHASE)
	#
	# HACK for Apple
	# check if library detection is required
	#
	if (APPLE AND DEFINED OPENSSL_INCLUDE_DIR AND DEFINED OPENSSL_LIBRARIES)
		set (OPENSSL_MANUAL_OSX_HACK on)
		set (OPENSSL_FOUND on)
	else ()
		set (OPENSSL_MANUAL_OSX_HACK off)
	endif ()

	if (APPLE AND DEFINED LIBGCRYPT_INCLUDE_DIR AND DEFINED LIBGCRYPT_LIBRARIES)
		set (LIBGCRYPT_MANUAL_OSX_HACK on)
		set (LIBGCRYPT_FOUND on)
	else ()
		set (LIBGCRYPT_MANUAL_OSX_HACK off)
	endif ()

	if (APPLE AND DEFINED BOTAN_INCLUDE_DIRS AND DEFINED BOTAN_LIBRARIES)
		set (BOTAN_MANUAL_OSX_HACK on)
		set (BOTAN_FOUND on)
	else ()
		set (BOTAN_MANUAL_OSX_HACK off)
	endif ()

	# Plugin variant: crypto_openssl
	set (plugin crypto_openssl)
	if (NOT OPENSSL_MANUAL_OSX_HACK)
		include (FindPkgConfig)
		pkg_search_module (OPENSSL openssl)
	endif ()

	if ((NOT OPENSSL_FOUND) AND (NOT OPENSSL_MANUAL_OSX_HACK))
		remove_plugin ( ${plugin} "OpenSSL development files not found")
	else  ()

		try_compile (HAS_OPENSSL_4SURE
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_openssl.c"
			CMAKE_FLAGS
				-DINCLUDE_DIRECTORIES:STRING=${OPENSSL_INCLUDE_DIR}
				-DLINK_LIBRARIES:PATH=${OPENSSL_LIBRARIES}
		)

		if (NOT HAS_OPENSSL_4SURE)
			remove_plugin ( ${plugin} "OpenSSL compilation test failed")
			set (OPENSSL_FOUND off)
		endif ()
	endif ((NOT OPENSSL_FOUND) AND (NOT OPENSSL_MANUAL_OSX_HACK))

	# Plugin variant: crypto_gcrypt
	set (plugin crypto_gcrypt)
	if (NOT LIBGCRYPT_MANUAL_OSX_HACK)
		find_package (LibGcrypt)
	endif ()

	if ((NOT LIBGCRYPT_FOUND) AND (NOT LIBGCRYPT_MANUAL_OSX_HACK))
		remove_plugin ( ${plugin} "libgcrypt development files not found")
	else ()

		try_compile (HAS_GCRYPT_4SURE
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_gcrypt.c"
			CMAKE_FLAGS
				-DINCLUDE_DIRECTORIES:STRING=${LIBGCRYPT_INCLUDE_DIR}
				-DLINK_LIBRARIES:PATH=${LIBGCRYPT_LIBRARIES}
		)

		if (NOT HAS_GCRYPT_4SURE)
			remove_plugin ( ${plugin} "libgcrypt compilation test failed")
			set (LIBGCRYPT_FOUND off)
		endif ()
	endif ((NOT LIBGCRYPT_FOUND) AND (NOT LIBGCRYPT_MANUAL_OSX_HACK))

	# Plugin variant: crypto_botan
	set (plugin crypto_botan)
	if (NOT BOTAN_MANUAL_OSX_HACK)
		include (FindPkgConfig)
		pkg_search_module (BOTAN botan-1.10 botan-1.9 botan-1.8 botan)
	endif ()

	if ((NOT BOTAN_FOUND) AND (NOT BOTAN_MANUAL_OSX_HACK))
		remove_plugin ( ${plugin} "botan development files not found")
	else ()

		try_compile (HAS_BOTAN_4SURE
			"${CMAKE_BINARY_DIR}"
			"${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_botan.cpp"
			CMAKE_FLAGS
				-DINCLUDE_DIRECTORIES:STRING=${BOTAN_INCLUDE_DIRS}
				-DLINK_LIBRARIES:PATH=${BOTAN_LIBRARIES}
		)

		if (NOT HAS_BOTAN_4SURE)
			remove_plugin ( ${plugin} "botan compilation test failed")
			set (BOTAN_FOUND off)
		endif ()
	endif ()

	# clean up for dependency phase
	unset (OPENSSL_MANUAL_OSX_HACK)
	unset (LIBGCRYPT_MANUAL_OSX_HACK)
	unset (BOTAN_MANUAL_OSX_HACK)
	unset (plugin)

	# add base64 directory to include path
	if(NOT "${OPENSSL_INCLUDE_DIR}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (OPENSSL_INCLUDE_DIR ${OPENSSL_INCLUDE_DIR} "${CMAKE_SOURCE_DIR}/src/plugins/base64/" CACHE STRING "OpenSSL include path" FORCE)
	endif ()

	if(NOT "${LIBGCRYPT_INCLUDE_DIR}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (LIBGCRYPT_INCLUDE_DIR ${LIBGCRYPT_INCLUDE_DIR} "${CMAKE_SOURCE_DIR}/src/plugins/base64/" CACHE STRING "libgcrypt include path" FORCE)
	endif ()

	if(NOT "${BOTAN_INCLUDE_DIRS}" MATCHES "${CMAKE_SOURCE_DIR}/src/plugins/base64/")
		set (BOTAN_INCLUDE_DIRS ${BOTAN_INCLUDE_DIRS} "${CMAKE_SOURCE_DIR}/src/plugins/base64/" CACHE STRING "Botan include path" FORCE)
	endif ()
endif ()

set (CRYPTO_COMMON_FILES ../base64/base64_functions.h ../base64/base64_functions.c helper.h helper.c gpg.h gpg.c crypto.h crypto.c)

#
# Compile Variant: OpenSSL
#
add_plugin (crypto_gcrypt
	SOURCES
		gcrypt_operations.h
		gcrypt_operations.c
		${CRYPTO_COMMON_FILES}
	INCLUDE_DIRECTORIES
		${LIBGCRYPT_INCLUDE_DIR}
	LINK_LIBRARIES
		${LIBGCRYPT_LIBRARIES}
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"crypto_gcrypt\"
		ELEKTRA_PLUGIN_NAME_C=crypto
		ELEKTRA_VARIANT=gcrypt
		ELEKTRA_CRYPTO_API_GCRYPT
)

#
# Compile Variant: libgcrypt
#
add_plugin (crypto_openssl
	SOURCES
		openssl_operations.h
		openssl_operations.c
		${CRYPTO_COMMON_FILES}
	INCLUDE_DIRECTORIES
		${OPENSSL_INCLUDE_DIR}
	LINK_LIBRARIES
		${OPENSSL_LIBRARIES}
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"crypto_openssl\"
		ELEKTRA_PLUGIN_NAME_C=crypto
		ELEKTRA_VARIANT=openssl
		ELEKTRA_CRYPTO_API_OPENSSL
)

#
# Compile Variant: Botan
#
add_plugin (crypto_botan
	SOURCES
		botan_operations.h
		botan_operations.cpp
		${CRYPTO_COMMON_FILES}
	INCLUDE_DIRECTORIES
		${BOTAN_INCLUDE_DIRS}
	LINK_LIBRARIES
		${BOTAN_LIBRARIES}
	COMPILE_DEFINITIONS
		ELEKTRA_PLUGIN_NAME=\"crypto_botan\"
		ELEKTRA_PLUGIN_NAME_C=crypto
		ELEKTRA_VARIANT=botan
		ELEKTRA_CRYPTO_API_BOTAN
)

if (ADDTESTING_PHASE)
	if (HAS_OPENSSL_4SURE)
		add_plugintest (crypto_openssl
				LINK_PLUGIN "crypto_openssl")
	endif ()

	if (HAS_GCRYPT_4SURE)
		add_plugintest (crypto_gcrypt
				MEMLEAK
				LINK_PLUGIN "crypto_gcrypt")
	endif ()

	if (HAS_BOTAN_4SURE)
		add_plugintest (crypto_botan
				LINK_PLUGIN "crypto_botan")
	endif ()
endif ()

unset (CRYPTO_COMMON_FILES)
