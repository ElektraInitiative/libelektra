#include <kdbmount.h>
#include <kdbhelper.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>





/* Read mount configuration from the KDB
 * Make sure to ksDel(...) the returned KeySet */
const KeySet * cReadMountConf (const bool clNull, const bool clFirst, const bool clSecond, const bool clThird, const bool clVerbose, const bool clDebug)
{
	Key * const errorKey = keyNew ("system:/elektra/mountpoints/error", KEY_END);
	KDB * const kdbHandle = kdbOpen (0, errorKey);

	/* TODO: 1st parameter is mountpointsPath (taken src/libs/tools/src/backends.cpp)
	 * --> define constant at better place! */
	Key * const parentKey = keyNew ("system:/elektra/mountpoints", KEY_END);
	KeySet * const mountConf = ksNew (0, KS_END); /* was a class variable in c++, now changed to return type */
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


void cOutputMtab (KeySet * const mountConf, bool clFirst, bool clSecond, bool clNull)
{
	// in c++: Vector with BackendInfo-structs
	struct cListBackendInfo * const mtab = cGetBackendInfo (mountConf);
	char delim = clNull ? '\0' : '\n';

	for (const struct cListBackendInfo * it = mtab; it; it = mtab->next)
	{
		if (clFirst)
		{
			printf ("%s", it->backendInfo.path);
			if (clSecond)
			{
				printf (" on ");
			}
			else
			{
				printf("%s%c", it->backendInfo.mountpoint, delim);
			}
		}

		if (clSecond)
		{
			printf ("%s%c", it->backendInfo.mountpoint, delim);
		}
	}

	freeListBackendInfo (mtab);
}


void cProcessArguments (bool clInteractive, int numArgs)
{
	if (!clInteractive && numArgs == 1)
	{
		/* TODO: Implement error handling */
		fprintf(stderr, "Wrong number of arguments, 0 or more than 1 needed!");
	}

	if (clInteractive)
	{
		puts ("Welcome to interactive mounting");
		puts ("Note that nothing will be made persistent");
		puts("until you say y at the very end of the mounting process\n");
	}
}


const char * cGetMountpoint (const KeySet * const mountconf, bool clInteractive)
{
	const Key * keyCur;

	if (clInteractive)
	{
		puts ("Already used are: ");

		for (elektraCursor it = 0; it < ksGetSize (mountconf); it++)
		{
			keyCur = ksAtCursor (mountconf, it);
			if (!strcmp (keyBaseName (keyCur), "mountpoint"))
			{
				printf ("%s ", keyString (keyCur));
			}
		}

		puts ("\nPlease start with / for a cascading backend");
		puts ("Enter the mountpoint: ");
		/* TODO: read user input
		 * C++:  cin >> mp; */
	}
	else
	{
		/* TODO: use GET_OPTION_KEY from PR #4438
		 * C++: mp = cl.createKey (1).getName (); */
	}

	/* TODO: return mp (string) */
}


void buildBackend (KeySet * const mountconf, const char * const mountpoint, bool clForce, const char * const clStrategy, bool clInteractive, const char * const pluginsconfig)
{
	const Key * const keyMountpoint = keyNew (mountpoint, KEY_END);

	/* TODO: implement Backend-related code (C++ classes) in C
	 * MountBackendBuilder backend; */

	if (!keyMountpoint)
	{
		/* TODO: Implement error handling
		 * throw invalid_argument (mp + " is not a valid mountpoint"); */
		return;
	}

	const KeySet * const ksDupMountConf = ksDup (mountconf);

	if (clForce || strcmp (clStrategy, "preserve") != 0)
	{
		/* TODO: 1st parameter is mountpointsPath (taken src/libs/tools/src/backends.cpp)
	 * --> define constant at better place! */
		Key * const cutKey = keyNew ("system:/elektra/mountpoints", KEY_END);
		keyAddBaseName (cutKey, mountpoint);
		KeySet * ksCutted = ksCut (mountconf, cutKey);
		/* We don't need the cut-out KeySet, but only the changed mountconf KeySet */
		ksDel (ksCutted);
	}

	/* C++: backend.setMountpoint (mpk, mountConf);
	 * backend.setBackendConfig (cl.getPluginsConfig ("system:/"));*/

}



/* Backend related stuff */

/** @brief Parse a string containing information to create a KeySet
 * @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 */
const KeySet * cParsePluginArguments (char * const pluginArguments, const char * const basepath)
{
	const KeySet * const ks = ksNew (0, KS_END);

	/* Read until the next '=', this should be the key name */
	size_t posEqual = strcspn (pluginArguments, "=");

	/* temporarly replace '=' with '\0' to use the current substring as parameter for a keyname */
	pluginArguments[posEqual] = 0;

	//const Key * const keyToAppend = keyNew ()

	return ks;
}


/* Give info about current mounted backends */
/* Make sure to free the returned list! */
/* The strings in the returned list are only valid as long as the given KeySet mountconf if valid! */
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
					else
					{
						/* TODO: Implement error handling */
						return NULL;
					}
				}

			}
		}
	}

	return biFirst;
}

void freeListBackendInfo (struct cListBackendInfo * const first)
{
	struct cListBackendInfo * prev;
	struct cListBackendInfo * cur;

	for (prev = first; prev; prev = cur)
	{
		cur = prev->next;
		free (prev);
	}
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
		const int isDirectlyBelow = keyIsDirectlyBelow (keyParent, keyCur);
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
