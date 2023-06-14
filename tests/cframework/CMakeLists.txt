include (SafeCheckSymbolExists)
include (LibAddMacros)
include (CMakePushCheckState)

add_definitions (-D_GNU_SOURCE -D_DARWIN_C_SOURCE)
safe_check_symbol_exists (nftw "ftw.h" HAVE_NFTW)
safe_check_symbol_exists (mkdtemp "stdlib.h;unistd.h" HAVE_MKDTEMP)
safe_check_symbol_exists (setenv "stdlib.h" HAVE_SETENV)

if (NOT (HAVE_MKDTEMP AND HAVE_SETENV))
	message (FATAL_ERROR "cframework requires mkdtemp && setenv, disable ENABLE_TESTING to build without testing")
endif (NOT (HAVE_MKDTEMP AND HAVE_SETENV))

if (HAVE_NFTW)
	add_definitions (-DUSE_NFTW)
endif (HAVE_NFTW)

set (SOURCES tests.c)
add_library (cframework OBJECT ${SOURCES})

set_target_properties (cframework PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
