include (CheckSymbolExists)
include (LibAddMacros)

check_symbol_exists (nftw "ftw.h" HAVE_NFTW)

if (HAVE_NFTW)
	add_definitions (-DUSE_NFTW)
endif (HAVE_NFTW)

set (SOURCES tests.c)
add_headers (SOURCES)
add_testheaders (SOURCES)

add_library (cframework OBJECT ${SOURCES})
add_dependencies (cframework kdberrors_generated)

set_target_properties (cframework PROPERTIES COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
