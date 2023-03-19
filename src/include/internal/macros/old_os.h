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

#endif /* KDBOS_H */
