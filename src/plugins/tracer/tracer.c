/***************************************************************************
            tracer.c  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _tracer functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "tracer.h"

int kdbOpen_tracer(KDB *handle)
{
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

	printf ("tracer: kdbOpen(%p)\n", (void*)handle);

	return 0;
}

int kdbClose_tracer(KDB *handle)
{
	/* free all backend resources and shut it down */

	printf ("tracer: kdbClose(%p)\n", (void*)handle);

	return 0; /* success */
}

ssize_t kdbGet_tracer(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: kdbGet(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s", keyName(k)); ++nr_keys; }
	printf (" %zd\n", nr_keys);

	return nr_keys; /* success */
}

ssize_t kdbSet_tracer(KDB *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: kdbSet(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s", keyName(k)); ++nr_keys; }
	printf (" %zd\n", nr_keys);

	return nr_keys;
}

KDB *KDBEXPORT(tracer)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_tracer,
		KDB_BE_CLOSE,	&kdbClose_tracer,
		KDB_BE_GET,	&kdbGet_tracer,
		KDB_BE_SET,	&kdbSet_tracer,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"The first plugin",
		KDB_BE_END);
}

