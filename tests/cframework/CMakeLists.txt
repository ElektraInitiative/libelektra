include (CheckSymbolExists)
include (LibAddMacros)
include (CMakePushCheckState)

# We need GNU_SOURCE and _XOEPN_SOURCE for full functinality
set (REQUIRED_FEATURE_MACROS -D_GNU_SOURCE -D_XOPEN_SOURCE=500 -D_DARWIN_C_SOURCE)
add_definitions (${REQUIRED_FEATURE_MACROS})

cmake_push_check_state (RESET)
set (CMAKE_REQUIRED_DEFINITIONS ${REQUIRED_FEATURE_MACROS})
check_symbol_exists (nftw "ftw.h" HAVE_NFTW)
cmake_pop_check_state ()

if (HAVE_NFTW)
	add_definitions (-DUSE_NFTW)
endif (HAVE_NFTW)

set (SOURCES tests.c)
add_headers (SOURCES)
add_testheaders (SOURCES)

add_library (cframework OBJECT ${SOURCES})
add_dependencies (cframework kdberrors_generated)

set_target_properties (cframework
		       PROPERTIES COMPILE_DEFINITIONS
				  HAVE_KDBCONFIG_H)
