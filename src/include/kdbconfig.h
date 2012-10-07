/***************************************************************************
      kdbconfig.h  -  Build system configuration

                           -------------------
    begin                : Apr 22 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

@DISCLAMER@

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef KDBCONFIG_H
#define KDBCONFIG_H

/**Below this directory the system configuration (system/) will be searched.*/
#define KDB_DB_SYSTEM            "@KDB_DB_SYSTEM@"

/** Depending on the resolver this might be the
  * postfix to search for user configuration. */
#define KDB_DB_USER              "@KDB_DB_USER@"

/** Depending on the resolver this might be the root 
  * to search for user configuration. */
#define KDB_DB_HOME              "@KDB_DB_HOME@"

/** Declares a parameter as unused. */
#define ELEKTRA_UNUSED           @ELEKTRA_UNUSED@

/** Declares an API as deprecated. */
#define ELEKTRA_DEPRECATED       @ELEKTRA_DEPRECATED@

/* disable debug output messages */
#define DEBUG @DEBUG@

/* disable verbose output messages */
#define VERBOSE @VERBOSE@

/* Define to 1 if you have the `clearenv' function. */
#define HAVE_CLEARENV @HAVE_CLEARENV@

/* Define to 1 if you have the <ctype.h> header file. */
#define HAVE_CTYPE_H @HAVE_CTYPE_H@

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H @HAVE_ERRNO_H@

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H @HAVE_LOCALE_H@

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV @HAVE_SETENV@

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H @HAVE_STDIO_H@

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H @HAVE_STDLIB_H@

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H @HAVE_STRING_H@

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H @HAVE_TIME_H@

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H @HAVE_UNISTD_H@

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif

#endif
