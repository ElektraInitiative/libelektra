include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	find_package (Libgcrypt QUIET)
	if (NOT LIBGCRYPT_FOUND)
		remove_plugin (crypto "libgcrypt development files not found")
	endif ()
endif ()

#
# Compile Variant: libgcrypt
#
add_plugin (
	crypto
	SOURCES gcrypt_operations.h
		gcrypt_operations.c
		helper.h
		helper.c
		gpg.h
		gpg.c
		crypto.h
		crypto.c
	INCLUDE_DIRECTORIES ${Libgcrypt_INCLUDE_DIRS}
	LINK_LIBRARIES ${Libgcrypt_LIBRARIES}
	LINK_ELEKTRA elektra-invoke
	ADD_TEST COMPONENT libelektra${SO_VERSION}-crypto)

if (ADDTESTING_PHASE)
	set_property (TEST "testmod_crypto" PROPERTY LABELS memleak)
endif (ADDTESTING_PHASE)
