/***************************************************************************
            dump.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon May  3 15:22:44 CEST 2010
    copyright            : by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include "dump.hpp"

namespace ckdb {
extern "C" {

int kdbOpen_dump(ckdb::KDB *handle)
{
	int errnosave = errno;
	/* backend initialization logic */

	errno = errnosave;
	return 0;
}

int kdbClose_dump(ckdb::KDB *handle)
{
	int errnosave = errno;
	/* free all backend resources and shut it down */

	errno = errnosave;
	return 0; /* success */
}

ssize_t kdbGet_dump(ckdb::KDB *handle, ckdb::KeySet *returned, const ckdb::Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

	/* get all keys below parentKey and count them with nr_keys */

	errno = errnosave;
	return nr_keys; /* success */
}

ssize_t kdbSet_dump(ckdb::KDB *handle, ckdb::KeySet *returned, const ckdb::Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

	/* set all keys below parentKey and count them with nr_keys */

	errno = errnosave;
	return nr_keys;
}

KDBEXPORT(dump)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_dump,
		KDB_BE_CLOSE,	&kdbClose_dump,
		KDB_BE_GET,	&kdbGet_dump,
		KDB_BE_SET,	&kdbSet_dump,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Full Name <email@libelektra.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"Add description here",
		KDB_BE_END);
}

} // extern C
}

