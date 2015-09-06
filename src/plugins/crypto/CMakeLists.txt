find_package (LibGcrypt)

set (SOURCES 
	gcrypt_operations.h
	gcrypt_operations.c
	crypto.h
	crypto.c
)

if (LIBGCRYPT_FOUND)
	include(LibAddPlugin)

	add_plugin(crypto
		SOURCES 
			${SOURCES}
		INCLUDE_DIRECTORIES
			${LIBGCRYPT_INCLUDE_DIR}
		LINK_LIBRARIES
			${LIBGCRYPT_LIBRARIES}
		)

	add_plugintest(crypto)
	
	install(DIRECTORY crypto DESTINATION ${TARGET_TEST_DATA_FOLDER})
	
else (LIBGCRYPT_FOUND)
	remove_plugin (crypto "libgcrypt not found")
endif (LIBGCRYPT_FOUND)
