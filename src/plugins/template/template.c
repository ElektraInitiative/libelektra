/***************************************************************************
            template.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid backend.                             *
 *   Simple fill the empty _template functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "template.h"

int kdbOpen_template(KDB *handle)
{
	int errnosave = errno;
	KDBCap *cap = kdbhGetCapability (handle);

	cap->onlyFullGet=1;
	cap->noStat=1;

	cap->onlyRemoveAll=1;

	cap->onlyFullSet=1;
	cap->onlyAddKeys=1;

	cap->onlySystem=1;
	cap->onlyUser=1;

	cap->noOwner=1;
	cap->noValue=1;
	cap->noComment=1;
	cap->noUID=1;
	cap->noGID=1;
	cap->noMode=1;
	cap->noDir=1;
	cap->noATime=1;
	cap->noMTime=1;
	cap->noCTime=1;
	cap->noRemove=1;
	cap->noMount=1;
	cap->noBinary=1;
	cap->noString=1;
	cap->noTypes=1;
	cap->noError=1;

	cap->noLock=1;
	cap->noThread=1;

	/* backend initialization logic */

	errno = errnosave;
	return 0;
}

int kdbClose_template(KDB *handle)
{
	int errnosave = errno;
	/* free all backend resources and shut it down */

	errno = errnosave;
	return 0; /* success */
}

ssize_t kdbGet_template(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

	/* get all keys below parentKey and count them with nr_keys */

	errno = errnosave;
	return nr_keys; /* success */
}

ssize_t kdbSet_template(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	int errnosave = errno;

	/* set all keys below parentKey and count them with nr_keys */

	errno = errnosave;
	return nr_keys;
}

KDB *KDBEXPORT(template)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_template,
		KDB_BE_CLOSE,	&kdbClose_template,
		KDB_BE_GET,	&kdbGet_template,
		KDB_BE_SET,	&kdbSet_template,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Full Name <email@libelektra.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"Add description here",
		KDB_BE_END);
}

