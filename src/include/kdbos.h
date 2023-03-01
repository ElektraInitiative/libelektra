/**
 * @file
 *
 * @brief Operating system specific workarounds
 *
 * Integer Types must be at least 32bit:
 *
 * Type     Purpose                 Limits
 * int      Integral Fast Type      INT_MIN, INT_MAX
 * size_t   size of array or string 0, SIZE_MAX
 * ssize_t  size with error cond.   -1, SSIZE_MAX(<SIZE_MAX)
 * time_t   Seconds since 1970      0,.. recommended: 64 bit
 *
 *
 * Following constants must be defined:
 *
 * KDB_FILE_MODE       the standard mode for keys
 * KDB_DIR_MODE        the mode to add (|=) for key directories
 *
 * Following limits must be defined:
 *
 * KDB_MAX_PATH_LENGTH the maximum length for a pathname
 *
 * In addition to the types the ... or va_list must be supported,
 * this is ISO C and should happen by including <stdarg.h>.
 *
 * Go ahead and write a #ifdef block for your operating system
 * when the POSIX defaults are not ok
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBOS_H
#define KDBOS_H
#ifndef KDB_H
#error you attempted to include kdbos.h outside from kdb.h, please include kdb.h instead
#endif

#ifdef __cplusplus
#define KS_END (static_cast<ckdb::Key *> (0))
#else
#define KS_END ((Key *) 0)
#endif


#ifdef __GNUC__
#undef ELEKTRA_SENTINEL
#define ELEKTRA_SENTINEL __attribute__ ((sentinel))
#endif

#ifdef __GNUC__
#undef ELEKTRA_WRONG
#define ELEKTRA_WRONG __attribute__ ((unused)) __attribute__ ((noinline)) __attribute__ ((error ("wrong usage of API")))
#endif

// must be later, clang has __GNUC__ set, too
#ifdef __clang__
#undef ELEKTRA_WRONG
#define ELEKTRA_WRONG __attribute__ ((unused)) __attribute__ ((noinline)) __attribute__ ((unavailable ("wrong usage of API")))
#endif

#ifdef __GNUC__
#define ELEKTRA_NOINLINE __attribute__ ((noinline))
#else
#define ELEKTRA_NOINLINE
#endif

#ifdef __GNUC__
#define ELEKTRA_WEAK __attribute__ ((weak))
#endif

#ifdef __clang__
#undef ELEKTRA_WEAK
#define ELEKTRA_WEAK __attribute__ ((weak))
#endif


/** The buffer size needed for an array name
 *
 * The size of the buffer so that the buffer can contain:
 * - (1) a # in the beginning
 * - (9) up to 19 underscores are needed as prefix
 * - (20) a 64bit number has a maximum of 20 digits
 * - (1) one byte for null termination
 *
 * E.g. \#___________________18446744073709551615\\0
 */
#define ELEKTRA_MAX_ARRAY_SIZE (1 + 19 + 20 + 1)

/**Default Mode.
 * This mode will be used for new files*/
#define KDB_FILE_MODE 0600

/**Default directory mode.
 * This mode will be used for new directories.
 * Will be ORed together with KDB_FILE_MODE
 * to get the permissions of an directory.*/
#define KDB_DIR_MODE 0100


#ifndef _WIN32

/***************************************************
 *               Posix Compatible
 ***************************************************/

#ifndef __USE_POSIX
#define __USE_POSIX
#endif
#include <limits.h>

/* Conforming C++ programs are not allowed to
 * include inttypes.h*/
#include <inttypes.h>
#include <sys/types.h>

#ifndef SSIZE_MAX
#ifdef _POSIX_SSIZE_MAX
#define SSIZE_MAX _POSIX_SSIZE_MAX
#else
#define SSIZE_MAX ((ssize_t) (SIZE_MAX / 2U))
#endif
#endif


/**KDB_MAX_PATH_LENGTH will be the value for longest
 * possible filenames on the system.*/

/*Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define KDB_MAX_PATH_LENGTH PATH_MAX
/*This value is guaranteed on any POSIX system*/
#elif defined __USE_POSIX
#define KDB_MAX_PATH_LENGTH _POSIX_PATH_MAX
#else
#define KDB_MAX_PATH_LENGTH 4096
#endif


#else /* _WIN32 */

/***************************************************
 *                 Windows (using mingw)
 ***************************************************/

/* Avoid the most crazy things */
#ifndef NOMINMAX
#define NOMINMAX
#include <windows.h>
#undef NOMINMAX
#else
#include <windows.h>
#endif

#include <limits.h>
#include <sys/types.h>

/* If MSVC use SSIZE_T type */
#ifdef _MSC_VER
#undef ssize_t
typedef SSIZE_T ssize_t;
#endif

#define KDB_MAX_PATH_LENGTH 4096

#endif /* _WIN32 */

/***************************************************
 *               For ANSI C systems
 ***************************************************/


/* Include essential headers used in kdb.h */
#include <stdarg.h>

#endif /* KDBOS_H */
