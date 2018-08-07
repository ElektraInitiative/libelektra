include (CheckSymbolExists)
include (LibAddMacros)
include (CMakePushCheckState)

# We need GNU_SOURCE and _XOEPN_SOURCE for full functinality
set (REQUIRED_FEATURE_MACROS -D_GNU_SOURCE -D_XOPEN_SOURCE=500 -D_DARWIN_C_SOURCE)
add_definitions (${REQUIRED_FEATURE_MACROS})

cmake_push_check_state (RESET)
set (CMAKE_REQUIRED_DEFINITIONS ${REQUIRED_FEATURE_MACROS})

set (CMAKE_C_FLAGS_OLD ${CMAKE_C_FLAGS})
# On some system the check for `ftw.h` will produce warnings. In that case the detection of `ftw.h` will fail, if we use the flag `-Werror`.
# We make sure that this is not the case by temporarily disabling `-Werror`.
string (REPLACE "-Werror"
		""
		CMAKE_C_FLAGS
		${CMAKE_C_FLAGS})

check_symbol_exists (nftw "ftw.h" HAVE_NFTW)

set (CMAKE_C_FLAGS ${CMAKE_C_FLAGS_OLD})
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
