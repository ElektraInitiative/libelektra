/***************************************************************************
             kdbos.h  -  operating system specific workarounds
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/* This header purpose is that afterwards following types are defined:
 * .. means don't care, just enough for your system
 * For more information on that types read POSIX documentation.
 *
 * Type     Purpose                 Limits
 * size_t   size of array or string 0, SIZE_MAX
 * ssize_t  size with error cond.   -1, SSIZE_MAX(<SIZE_MAX)
 * time_t   Seconds since 1970      0,.. recommended: 64 bit
 *
 * Integer Types must be at least 32bit:
 *
 * int      Integral Fast Type      INT_MIN, INT_MAX
 * uid_t    User identification     0,..
 * gid_t    Group identification    0,..
 * keyswitch_t For keyNew
 * option_t    For kdbGet, kdbSet and ksLookup*
 *
 *
 * Following elektra specific types are defined:
 *
 * Type     Purpose
 * cursor_t stores information to find a position in a keyset
 *
 * Following constants must be defined:
 *
 * KEY_SEPARATOR   how to delimit keynames
 * PATH_SEPARATOR  how to delimit pathnames
 * KEY_DEF_MODE    the standard mode for keys
 * KEY_DIR_MODE    the mode to add (|=) for key directories
 *
 * Following limits must be defined (in addition to limits mentioned
 * above for types):
 *
 * MAX_UCHAR       the maximum for unsigned char
 * MAX_KEY_LENGTH  the maximum length for a keyname
 * MAX_PATH_LENGTH the maximum length for a pathname
 *
 * In addition to the types the ... or va_list must be supported,
 * this is ISO C and should happen by including <stdarg.h>.
 *
 * Go ahead and write a #ifdef block for your operating system
 * when the POSIX defaults are not ok.
 */


#ifndef KDBOS_H
#define KDBOS_H


/* Include essential headers used in kdb.h */
#include <stdarg.h>

#ifndef WIN32

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

/**Separator for Path names*/
#define PATH_SEPARATOR '/'


/**MAX_PATH_LENGTH will be the value for longest
 * possible filenames on the system.*/

/*Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/*This value is garanteed on any Posixsystem*/
#elif defined __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else
#define MAX_PATH_LENGTH 4096
#endif

/*Type to point to every position within the keyset*/
typedef ssize_t cursor_t;

/*Integer types*/
typedef int keyswitch_t;
typedef int option_t;

/**Separator for key names.
 * This character will be used to separate key names*/
#define KEY_SEPARATOR '/'
/**Default Mode.
 * This mode will be used for fresh keys*/
#define KEY_DEF_MODE 0664
/**Default directory mode.
 * This mode will be ORed to have a directory key*/
#define KEY_DEF_DIR 0111

/**Lets assume that the namespace + relative path is shorter then the absolute path.
 * So the maximum length of key is what the system allows for the path.*/
#define MAX_KEY_LENGTH MAX_PATH_LENGTH




#else /* WIN32 */

/***************************************************
 *                 Windows
 ***************************************************/
# include <windows.h>
# include <limits.h>

# define usleep(x) Sleep(x)

# define ssize_t int
# define snprintf _snprintf

/**Separator for Path names*/
#define PATH_SEPARATOR '\\'

#define MAX_PATH_LENGTH 4096

/*Type to point to every position within the keyset*/
typedef ssize_t cursor_t;

/*Integer types*/
typedef int keyswitch_t;
typedef int option_t;

/**Separator for key names.
 * This character will be used to separate key names*/
#define KEY_SEPARATOR '/'
/**Default Mode.
 * This mode will be used for fresh keys*/
#define KEY_DEF_MODE 0664
/**Default directory mode.
 * This mode will be ORed to have a directory key*/
#define KEY_DEF_DIR 0111

/**Lets assume that the namespace + relative path is shorter then the absolute path.
 * So the maximum length of key is what the system allows for the path.*/
#define MAX_KEY_LENGTH MAX_PATH_LENGTH




#endif /* WIN32 */

#endif /* KDBOS_H */

