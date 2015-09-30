find_package (LibGcrypt)

if (LIBGCRYPT_FOUND)
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

	add_plugin(crypto
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

	add_plugintest(crypto)

else (LIBGCRYPT_FOUND)
	remove_plugin (crypto "libgcrypt not found")
endif (LIBGCRYPT_FOUND)
