include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	set (plugin crypto)
	find_package (Libgcrypt QUIET)
	if (NOT LIBGCRYPT_FOUND)
		remove_plugin (${plugin} "libgcrypt development files not found")
	endif ()

	# clean up for dependency phase
	unset (plugin)
endif ()

#
# Compile Variant: libgcrypt
#
add_plugin (
	crypto
	SOURCES gcrypt_operations.h gcrypt_operations.c helper.h helper.c gpg.h gpg.c crypto.h crypto.c
	INCLUDE_DIRECTORIES ${Libgcrypt_INCLUDE_DIRS}
	LINK_LIBRARIES ${Libgcrypt_LIBRARIES}
	LINK_ELEKTRA elektra-invoke
	ADD_TEST)
