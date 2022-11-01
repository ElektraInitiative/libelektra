#include <kdbmount.h>
#include <kdbhelper.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>





/* Read mount configuration from the KDB
 * Make sure to ksDel(...) the returned KeySet */
const KeySet * cReadMountConf (const bool clNull, const bool clFirst, const bool clSecond, const bool clThird, const bool clVerbose, const bool clDebug)
{
	/* TODO: 1st parameter is mountpointsPath (taken src/libs/tools/src/backends.cpp)
	 * --> define constant at better place! */
	Key * const errorKey = keyNew ("system:/elektra/mountpoints/error");
	KDB * const kdbHandle = kdbOpen (0, errorKey);


	Key * const parentKey = keyNew ("system:/elektra/mountpoints", KEY_END);
	KeySet * const mountConf = ksNew (0); /* was a class variable in c++, now changed to return type */
	const int ret = kdbGet (kdbHandle, mountConf, parentKey);

	kdbClose (kdbHandle, errorKey);
	keyDel (errorKey);

	if (ret == -1)
	{
		/* TODO: Implement error handling */
	}

	if (!clNull && clFirst && clSecond && clThird)
		cPrintWarnings (parentKey, clVerbose, clDebug);

	keyDel (parentKey);

	return mountConf;
}


void cOutputMtab (const KeySet * const mountConf, bool clFirst, bool clSecond, bool clNull)
{
	// in c++: Vector with BackendInfo-structs

}



/* Backend related stuff */

/* Give info about current mounted backends */
/* Make sure to free the returned list! */
struct cListBackendInfo * cGetBackendInfo (KeySet * const mountConf)
{
	/* TODO: 1st parameter is mountpointsPath (taken from src/libs/tools/src/backends.cpp)
	 * --> define constant at better place! */
	struct cListBackendInfo * biFirst = NULL, * biCurrent = NULL;
	const Key * const keyRoot = keyNew ("system:/elektra/mountpoints", KEY_END);

	for (elektraCursor it = 0; it < ksGetSize (mountConf); it++)
	{
		const Key * const keyCur = ksAtCursor (mountConf, it);

		if (keyIsDirectlyBelow (keyRoot, keyCur) == 1)
		{
			/* keyCur is directly below keyRoot */
			struct cBackendInfo bi;

			/* -1 because '\0' is counted for both strings */
			size_t lenStrPath = elektraStrLen (keyName (keyCur)) + elektraStrLen ("/definition/path") - 1;
			char * const strPath = elektraMalloc (lenStrPath);
			if (strPath)
			{
				strcpy (strPath, keyName (keyCur));
				strcat (strPath, "/definition/path");
				const Key * const keyPath = ksLookupByName (mountConf, strPath, KDB_O_NONE);
				if (keyPath)
				{
					bi.path = keyString (keyPath);
				}
				bi.mountpoint = keyBaseName (keyCur);
				if (biCurrent)
				{
					/* TODO: assert biCurrent->next is NULL */
					biCurrent->next = elektraMalloc (sizeof (struct  cListBackendInfo));
					biCurrent = biCurrent->next;
					biCurrent->next = NULL;
					biCurrent->backendInfo = bi;
				}
				else
				{
					biFirst = elektraMalloc (sizeof (struct cListBackendInfo));
					if (biFirst)
					{
						biFirst->backendInfo = bi;
						biFirst->next = NULL;
						biCurrent = biFirst;
					}
				}

			}
		}
	}

	return biFirst;
}




















/* The following code is not directly used for mounting, but was c++-code that was called from the c++-code for mounting */

/* Currently prints directly to stderr instead of taking an output stream as parameter
 * TODO: Implement colored output (see src/tools/kdb/ansicolors.hpp) */
void cPrintWarnings (Key * const error, const bool printVerbose, const bool printDebug)
{
	KeySet * const keyErrorMeta = ksDup (keyMeta (error));
	Key * const keyParent = keyNew ("meta:/warnings", KEY_END);
	KeySet * const ksWarnings = ksCut (keyErrorMeta, keyParent);
	ksDel (keyErrorMeta);
	keyDel (keyParent);

	/* TODO: Implement error handling */
	if (!ksWarnings || ksGetSize (ksWarnings) == 0)
	{
		if (ksWarnings)
			ksDel (ksWarnings);
		return;
	}

	/* Get number of warnings */
	const Key * const keyMetaWarnings = ksLookupByName (ksWarnings, "meta:/warnings", KDB_O_NONE);
	long cntWarnings = 0;
	if (keyMetaWarnings)
	{
		const char * const strWarningCount = keyString (keyMetaWarnings);

		/* skip leading '#' and '_' characters */
		for (size_t i = 0; i < elektraStrLen (strWarningCount) && (*(strWarningCount+i) == '#' || *(strWarningCount+i) == '-'); i++);
		/* no loop body! */

		char * eptr;
		cntWarnings = strtol (strWarningCount, &eptr, 10);

		if (*eptr)
		{
			/* String could not be fully converted to a long
			 * TODO: implement error handling */
			ksDel (ksWarnings);
			 return;
		}
	}


	/* TODO: Maybe implement special handling for 0 warnings (not present in original c++ code) */
	fprintf (stderr, "Sorry, %ld warning%s issued :(\n", cntWarnings, cntWarnings == 1 ? " was" : "s were");

	cntWarnings = 0;
	for (elektraCursor i = 0; i < ksGetSize (ksWarnings); i++)
	{
		Key * keyCur = ksAtCursor (ksWarnings, i);
		const int isDirectlyBelow = keyIsDirectlyBelow (ksWarnings, keyCur);
		if (isDirectlyBelow == -1)
		{
			/* TODO: Implement error handling or treat as false (TDB) */
			ksDel (ksWarnings);
			return;
		}
		else if (isDirectlyBelow == 1) /* directly below */
		{
			const char * const curKeyName = keyName (keyCur);


			const size_t lenCurKeyName = elektraStrLen (curKeyName);
			/* Subtract 1 because elektraStrLen(...) counts '\0' of both strings (two function calls),
			 * use the string "description" for calculating the size, as this is the longest subkey-name we need. */
			const size_t searchKeyNameSize = lenCurKeyName + elektraStrLen ("/description") - 1;
			char * const searchKeyName = elektraMalloc (searchKeyNameSize);
			if (searchKeyName)
			{
				strcpy (searchKeyName, curKeyName);
				strcat (searchKeyName, "/module");
				const Key * const keyModule = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "number");
				const Key * const keyNumber = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "reason");
				const Key * const keyReason = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "description");
				const Key * const keyDescription = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

				fprintf (stderr, " %zd: Module %s issued the warning %s:\n", ++cntWarnings, keyString (keyModule),
					 keyString (keyNumber));
				fprintf (stderr, "\t%s: %s\n", keyString (keyDescription), keyString (keyReason));

				if (printVerbose)
				{
					strcpy (searchKeyName + lenCurKeyName, "mountpoint");
					const Key * const keyMountpoint = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
					strcpy (searchKeyName + lenCurKeyName, "configfile");
					const Key * const keyConfigFile = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

					fprintf (stderr, "\tMountpoint: %s\n", keyString (keyMountpoint));
					fprintf (stderr, "\tConfigfile: %s\n", keyString (keyConfigFile));
				}

				if (printDebug)
				{
					strcpy (searchKeyName + lenCurKeyName, "file");
					const Key * const keyFile = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
					strcpy (searchKeyName + lenCurKeyName, "line");
					const Key * const keyLine = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

					fprintf (stderr, "\tAt: %s:%s\n", keyString (keyFile), keyString (keyLine));
				}
				elektraFree (searchKeyName);
			}
		}
	}

	fflush (stderr);
	ksDel (ksWarnings);
}