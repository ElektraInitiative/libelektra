# Find Module for Libgcrypt
# =========================
#
# This module defines the variables listed below.
#
# - Libgcrypt_FOUND:        This variable will be set to a value that evaluates to true, if the system includes Libgcrypt.
# - Libgcrypt_INCLUDE_DIRS: This variable stores the Libgcrypt include directory.
# - Libgcrypt_LIBRARIES:    This variable stores the libraries needed to link to Libgcrypt.

find_path (Libgcrypt_INCLUDE_DIR NAMES gcrypt.h)

find_library (Libgcrypt_LIBRARY NAMES gcrypt)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Libgcrypt REQUIRED_VARS Libgcrypt_LIBRARY Libgcrypt_INCLUDE_DIR)

mark_as_advanced (Libgcrypt_INCLUDE_DIR Libgcrypt_LIBRARY Libgcrypt_VERSION)

set (Libgcrypt_LIBRARIES ${Libgcrypt_LIBRARY})
set (Libgcrypt_INCLUDE_DIRS ${Libgcrypt_INCLUDE_DIR})

if (Libgcrypt_FOUND)
	# Try to compile and link a minimal sample program against libgcrypt
	try_compile (
		HAS_GCRYPT_4SURE ${CMAKE_BINARY_DIR}
		${PROJECT_SOURCE_DIR}/src/plugins/crypto/compile_gcrypt.c
		CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${Libgcrypt_INCLUDE_DIRS} -DLINK_LIBRARIES:PATH=${Libgcrypt_LIBRARIES})

	if (NOT HAS_GCRYPT_4SURE)
		message (STATUS "libgcrypt compile/linker test failed. Please check if all library and include paths are set properly!")
	endif ()
endif (Libgcrypt_FOUND)
