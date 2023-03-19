#ifndef ELEKTRA_INTERNAL_MACROS_OS_H
#define ELEKTRA_INTERNAL_MACROS_OS_H

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

#endif // ELEKTRA_PLUGIN_H
