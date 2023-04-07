include (CheckIncludeFile)
include (SafeCheckSymbolExists)
include (CheckTypeSize)
include (TestBigEndian)
include (LibAddBinding)

safe_check_symbol_exists (clearenv "stdlib.h" HAVE_CLEARENV)
safe_check_symbol_exists (setenv "stdlib.h" HAVE_SETENV)
safe_check_symbol_exists (futimens "sys/stat.h" HAVE_FUTIMENS)

add_definitions (-D_GNU_SOURCE -D_DARWIN_C_SOURCE)
safe_check_symbol_exists (hsearch_r "search.h" HAVE_HSEARCHR)

safe_check_symbol_exists (futimes "sys/time.h" HAVE_FUTIMES)
safe_check_symbol_exists (glob "glob.h" HAVE_GLOB)
safe_check_symbol_exists (clock_gettime "time.h" HAVE_CLOCK_GETTIME)

check_include_file (ctype.h HAVE_CTYPE_H)
check_include_file (errno.h HAVE_ERRNO_H)
check_include_file (features.h HAVE_FEATURES_H)
check_include_file (locale.h HAVE_LOCALE_H)
check_include_file (stdio.h HAVE_STDIO_H)
check_include_file (stdlib.h HAVE_STDLIB_H)
check_include_file (string.h HAVE_STRING_H)
check_include_file (time.h HAVE_TIME_H)
check_include_file (unistd.h HAVE_UNISTD_H)

check_type_size (int SIZEOF_INT)
check_type_size (long SIZEOF_LONG)
check_type_size (size_t SIZEOF_SIZE_T)
check_type_size ("long long" SIZEOF_LONG_LONG)
check_type_size ("long double" SIZEOF_LONG_DOUBLE)
check_type_size (mode_t SIZEOF_MODE_T)
check_type_size (float SIZEOF_FLOAT)
check_type_size (double SIZEOF_DOUBLE)

set (CMAKE_EXTRA_INCLUDE_FILES "sys/time.h")
check_type_size ("((struct timeval*)0)->tv_usec" SIZEOF_TV_USEC)
set (CMAKE_EXTRA_INCLUDE_FILES)

set (CMAKE_EXTRA_INCLUDE_FILES "sys/stat.h")
check_type_size ("((struct stat*)0)->st_size" SIZEOF_STAT_ST_SIZE)
set (CMAKE_EXTRA_INCLUDE_FILES)

if (SIZEOF_LONG EQUAL 4)
	set (INT32_T "long")
	set (UINT32_T "unsigned long")
	set (PRI_I32 "\"ld\"")
	set (PRI_U32 "\"lu\"")
elseif (SIZEOF_INT EQUAL 4)
	set (INT32_T "int")
	set (UINT32_T "unsigned int")
	set (PRI_I32 "\"d\"")
	set (PRI_U32 "\"u\"")
else ()
	message (FATAL_ERROR "Couldn't find a 32-bit integer type for use as kdb_long_t")
endif ()

if (SIZEOF_LONG EQUAL 8)
	set (INT64_T "long")
	set (UINT64_T "unsigned long")
	set (PRI_I64 "\"ld\"")
	set (PRI_U64 "\"lu\"")
	set (STRTOLL "strtol")
	set (STRTOULL "strtoul")
elseif (HAVE_SIZEOF_LONG_LONG AND SIZEOF_LONG_LONG EQUAL 8)
	set (INT64_T "long long")
	set (UINT64_T "unsigned long long")
	set (PRI_I64 "\"lld\"")
	set (PRI_U64 "\"llu\"")
	set (STRTOLL "strtoll")
	set (STRTOULL "strtoull")
else ()
	message (FATAL_ERROR "Couldn't find a 64-bit integer type for use as kdb_long_long_t")
endif ()

if (HAVE_SIZEOF_LONG_DOUBLE AND SIZEOF_LONG_DOUBLE GREATER 9)
	set (ELEKTRA_HAVE_KDB_LONG_DOUBLE "1")
endif ()

if (SIZEOF_TV_USEC EQUAL SIZEOF_LONG)
	set (ELEKTRA_TIME_USEC_F "\"%ld\"")
elseif (SIZEOF_TV_USEC EQUAL SIZEOF_LONG_LONG)
	set (ELEKTRA_TIME_USEC_F "\"%lld\"")
else ()
	set (ELEKTRA_TIME_USEC_F "\"%d\"")
endif ()

# Unfortunately `long` and `long long` have the same size (8 bytes) on `amd64` architectures in macOS and Debian GNU/Linux. This means we
# can not differentiate these types only based on their size. Consequently we set the `long long` format specifier for `st_size` on macOS,
# and hope that all other operating systems, either use differently sized types for `long` and `long long`, or just use `long` as data type
# for `st_size`.
if (APPLE)
	set (ELEKTRA_STAT_ST_SIZE_F "\"%llu\"")
elseif (SIZEOF_STAT_ST_SIZE EQUAL SIZEOF_LONG)
	set (ELEKTRA_STAT_ST_SIZE_F "\"%lu\"")
elseif (SIZEOF_STAT_ST_SIZE EQUAL SIZEOF_LONG_LONG)
	set (ELEKTRA_STAT_ST_SIZE_F "\"%llu\"")
else ()
	set (ELEKTRA_STAT_ST_SIZE_F "\"%u\"")
endif ()

if (NOT SIZEOF_FLOAT EQUAL 4)
	message (FATAL_ERROR "float must be a 32-bit type")
endif ()

if (NOT SIZEOF_DOUBLE EQUAL 8)
	message (FATAL_ERROR "double must be a 64-bit type")
endif ()

if (SIZEOF_SIZE_T EQUAL 8)
	set (KDB_OPMPHM_MAX_N "4294967295") # = (2^32) - 1
else ()
	set (KDB_OPMPHM_MAX_N "795364313") # bound by opmphm->size max value
endif ()

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdbtypes.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdbtypes.h")

unset (INT32_T)
unset (UINT32_T)
unset (INT64_T)
unset (UINT64_T)
unset (PRI_I32)
unset (PRI_U32)
unset (PRI_I64)
unset (PRI_U64)
unset (STRTOLL)
unset (STRTOULL)

set (BUILTIN_EXEC_FOLDER "${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}")
set (BUILTIN_DATA_FOLDER "${CMAKE_INSTALL_PREFIX}/${TARGET_TEST_DATA_FOLDER}")
set (BUILTIN_PLUGIN_FOLDER "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER}")
set (ELEKTRA_DEPRECATED "__attribute__ ((deprecated))")
set (ELEKTRA_UNUSED "__attribute__ ((unused))")
set (ELEKTRA_FALLTHROUGH "__attribute__ ((fallthrough))")

# set ELEKTRA_NO_SANATIZE_*
if (ENABLE_ASAN)
	if (CMAKE_C_COMPILER_ID MATCHES "Clang") # clang section
		set (ELEKTRA_NO_SANITIZE_INTEGER "__attribute__ ((no_sanitize (\"integer\")))")
		set (ELEKTRA_NO_SANITIZE_UNSIGNED_INTEGER_OVERFLOW "__attribute__ ((no_sanitize (\"unsigned-integer-overflow\")))")
		set (ELEKTRA_NO_SANITIZE_UNDEFINED "__attribute__ ((no_sanitize (\"undefined\")))")
		set (ELEKTRA_NO_SANITIZE_ADDRESS "__attribute__ ((no_sanitize (\"address\")))")
		set (ELEKTRA_NO_SANITIZE_IMPLICIT_UNSIGNED_INTEGER_TRUNCATION
		     "__attribute__ ((no_sanitize (\"implicit-unsigned-integer-truncation\")))")
	elseif (CMAKE_C_COMPILER_ID MATCHES "GNU") # others section
		set (ELEKTRA_NO_SANITIZE_INTEGER "")
		set (ELEKTRA_NO_SANITIZE_UNSIGNED_INTEGER_OVERFLOW "")
		set (ELEKTRA_NO_SANITIZE_UNDEFINED "__attribute__ ((no_sanitize_undefined))")
		set (ELEKTRA_NO_SANITIZE_ADDRESS "__attribute__ ((no_sanitize_address))")
		set (ELEKTRA_NO_SANITIZE_IMPLICIT_UNSIGNED_INTEGER_TRUNCATION "")
	endif (CMAKE_C_COMPILER_ID MATCHES "Clang")
else (ENABLE_ASAN)
	set (ELEKTRA_NO_SANITIZE_INTEGER "")
	set (ELEKTRA_NO_SANITIZE_UNSIGNED_INTEGER_OVERFLOW "")
	set (ELEKTRA_NO_SANITIZE_UNDEFINED "")
	set (ELEKTRA_NO_SANITIZE_ADDRESS "")
	set (ELEKTRA_NO_SANITIZE_IMPLICIT_UNSIGNED_INTEGER_TRUNCATION "")
endif (ENABLE_ASAN)

if (ENABLE_OPTIMIZATIONS)
	set (ELEKTRA_ENABLE_OPTIMIZATIONS "1")
endif (ENABLE_OPTIMIZATIONS)

test_big_endian (ELEKTRA_BIG_ENDIAN)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdb.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdb.h")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdbconfig.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdbconfig.h")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdbversion.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h")

# DO NOT install kdbconfig.h it may cause conflicts with applications own config.h files
install (
	FILES kdbextension.h
	      kdbmeta.h
	      kdbease.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdbtypes.h"
	      kdbhelper.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdb.h"
	      kdbmodule.h
	      kdbos.h
	      kdbopts.h
	      kdbplugin.h
	      kdbpluginprocess.h
	      kdbprivate.h
	      kdbinvoke.h
	      kdbutility.h
	      kdbio.h
	      kdbnotification.h
	      kdbglobbing.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h"
	      elektra.h
	      kdbendian.h
	      kdbmacros.h
	      kdbmerge.h
	      kdbgopts.h
	      kdbmount.h
	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	COMPONENT libelektra-dev)

add_custom_target (elektra_config_headers ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/kdb.h ${CMAKE_CURRENT_BINARY_DIR}/kdbconfig.h
						      ${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h)

add_subdirectory (kdbio)
add_subdirectory (elektra)
