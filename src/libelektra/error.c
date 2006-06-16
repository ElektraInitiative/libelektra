/***************************************************************************
            error.c  -  Methods for error manipulation
                             -------------------
    begin                : Mon Mar 20 2006
    copyright            : (C) 2006 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/* Subversion stuff

$Id$

*/

#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "kdb.h"


/**
 * Provides an error string associated with @p errnum.
 * @return an error string associated with @p errnum.
 * @see kdbPrintError()
 * @ingroup kdb
 */
char *kdbStrError(int errnum) {
	switch (errnum) {
		case KDB_RET_INVALIDKEY:
			return "Invalid Key";
		case KDB_RET_NOKEY:
			return "Key has no name";
		case KDB_RET_NODATA:
			return "Key has no value";
		case KDB_RET_NODESC:
			return "Key has no comment/description";
		case KDB_RET_NODOMAIN:
			return "Key has no user domain";
		case KDB_RET_NOGROUP:
			return "Key has no group";
		case KDB_RET_NOTIME:
			return "Key has no access time";
		case KDB_RET_TYPEMISMATCH:
			return "Operation incompatible with key value type";
		case KDB_RET_NOSYS:
			return "Backend method not implemented";
		case KDB_RET_EBACKEND:
			return "Error opening backend";
		default:
			return strerror(errnum);
	}
}

/**
 * Prints an error message related to @c errno on standard error, prefixed by @p msg.
 * @see kdbStrError()
 * @see example on kdbGetKeyChildKeys()
 * @ingroup kdb
 */
int kdbPrintError(const char * msg) {
	fprintf (stderr, "%s: %s\n", msg, kdbStrError(errno));
	return 0;
}
