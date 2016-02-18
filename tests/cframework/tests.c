/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <string.h>
#include <tests.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include <kdbinternal.h>


int nbError;
int nbTest;

uid_t nbUid;
gid_t nbGid;

char file[KDB_MAX_PATH_LENGTH];
char srcdir[KDB_MAX_PATH_LENGTH];

#ifdef HAVE_CLEARENV
int clearenv ();
#endif

char * tmpfilename;
char * tempHome;
int tempHomeLen;
char * tempHomeConf;

static void clean_temp_home (void);

/**Does some useful startup.
 */
int init (int argc, char ** argv)
{
	char * tmpvar;
	int fd;

	setlocale (LC_ALL, "");

#ifdef HAVE_CLEARENV
	clearenv ();
#else
	unsetenv ("HOME");
	unsetenv ("USER");
#endif

	nbUid = getuid ();
	nbGid = getgid ();

	if (argc > 1)
	{
		strncpy (srcdir, argv[1], sizeof (srcdir));
	}
	else
	{
		strncpy (srcdir, BUILTIN_DATA_FOLDER, sizeof (srcdir));
	}

	tmpvar = getenv ("TMPDIR");
	if (!tmpvar)
	{
		tmpvar = "/tmp";
	}
	// check tempvar for trailing slash /
	if (strlen (tmpvar) > 2)
	{
		if (tmpvar[strlen (tmpvar) - 1] == '/')
		{
			tmpvar[strlen (tmpvar) - 1] = '\0';
		}
	}

	tempHomeLen = strlen (tmpvar) + 1 + 13 + 6 + 1;
	tempHome = elektraMalloc (tempHomeLen);
	tempHomeConf = elektraMalloc (tempHomeLen + strlen (KDB_DB_USER) + 2);
	succeed_if (tempHome != 0, "elektraMalloc failed");
	snprintf (tempHome, tempHomeLen, "%s/elektra-test.XXXXXX", tmpvar);
	snprintf (tempHomeConf, tempHomeLen, "%s/elektra-test.XXXXXX/" KDB_DB_USER, tmpvar);
	succeed_if (mkdtemp (tempHome) != 0, "mkdtemp failed");
	setenv ("HOME", tempHome, 1);

	atexit (clean_temp_home);

	int tmpfilenameLen = tempHomeLen + 1 + 12 + 6 + 1;
	tmpfilename = elektraMalloc (tmpfilenameLen);
	succeed_if (tmpfilenameLen != 0, "elektraMalloc failed");
	snprintf (tmpfilename, tmpfilenameLen, "%s/elektra-tmp.XXXXXX", tempHome);
	fd = mkstemp (tmpfilename);
	succeed_if (fd != -1, "mkstemp failed");
	close (fd);

	return 0;
}

/**Create a root key for a backend.
 *
 * @return a allocated root key */
Key * create_root_key (const char * backendName)
{
	Key * root = keyNew ("user/tests", KEY_END);
	/*Make mount point beneath root, and do all tests here*/
	/* Not needed anymore:
	keySetDir(root);
	keySetUID(root, nbUid);
	keySetGID(root, nbGid);
	keySetComment (root, "backend root key for tests");
	*/
	keyAddBaseName (root, backendName);
	keySetString (root, backendName);
	keySetString (root, backendName);
	return root;
}

/**Create a configuration keyset for a backend.
 *
 * @return a allocated configuration keyset for a backend*/
KeySet * create_conf (const char * filename)
{
	return ksNew (2, keyNew ("system/path", KEY_VALUE, filename, KEY_END), KS_END);
}


/**
 * @brief Compare two files line by line
 *
 * @param filename first file
 * @param genfilename file to compare with
 *
 * @retval 0 on errors (succeed_if already executed)
 * @retval 1 on success
 */
int compare_line_files (const char * filename, const char * genfilename)
{
	FILE *forg, *fgen;
	char bufferorg[BUFFER_LENGTH + 1];
	char buffergen[BUFFER_LENGTH + 1];
	char * org = 0;
	char * gen = 0;
	int line = 0;

	forg = fopen (filename, "r");
	fgen = fopen (genfilename, "r");

	strncpy (bufferorg, "could not open file, orig: ", BUFFER_LENGTH);
	strncat (bufferorg, filename, BUFFER_LENGTH);
	strncat (bufferorg, " gen: ", BUFFER_LENGTH);
	strncat (bufferorg, genfilename, BUFFER_LENGTH);

	exit_if_fail (forg && fgen, bufferorg);

	while ((org = fgets (bufferorg, BUFFER_LENGTH, forg)) && (gen = fgets (buffergen, BUFFER_LENGTH, fgen)))
	{
		line++;
		if (strncmp (bufferorg, buffergen, BUFFER_LENGTH))
		{
			printf ("Compare <%s>, with <%s>\n", bufferorg, buffergen);
			printf ("in file %s, line %d.\n", filename, line);
			succeed_if (0, "comparing lines failed");
			goto error;
		}
	}

	if (org || fgets (buffergen, BUFFER_LENGTH, fgen))
	{
		printf ("The files do not have same number of lines (%d): %s.\n", line, filename);
		succeed_if (0, "comparing files failed");
		goto error;
	}

	fclose (forg);
	fclose (fgen);
	return 1;

error:
	fclose (forg);
	fclose (fgen);
	return 0;
}


/**Compare two files line by line.
 *
 * Fails when there are any differences.
 *
 * The original file is passed as parameter.
 * It will be compared with the file -gen.
 *
 * file.xml -> file-gen.xml (xml comparator)
 * file.txt -> file-gen.txt (line comparator)
 * file.c -> file-gen.c (c comparator)
 *
 */
int compare_files (const char * filename)
{
	char genfilename[KDB_MAX_PATH_LENGTH];
	char * dot = strrchr (filename, '.');

	exit_if_fail (dot != 0, "could not find extension in file");

	strncpy (genfilename, filename, dot - filename);
	/* does not terminate string, but strcat need it, so: */
	genfilename[dot - filename] = 0;
	strcat (genfilename, "-gen");
	if (!strcmp (dot, ".xml"))
	{
		strcat (genfilename, ".xml");
	}
	else if (!strcmp (dot, ".txt"))
	{
		strcat (genfilename, ".txt");
	}
	else if (!strcmp (dot, ".c"))
	{
		strcat (genfilename, ".c");
	}

	return compare_line_files (filename, genfilename);
}


/* return file name in srcdir.
 * No bound checking on file size, may overflow. */
char * srcdir_file (const char * fileName)
{
	strcpy (file, srcdir);
	strcat (file, "/");
	strcat (file, fileName);
	return file;
}

const char * elektraFilename ()
{
	return tmpfilename;
}

void elektraUnlink (const char * filename)
{
	unlink (filename);
}

void clear_sync (KeySet * ks)
{
	Key * k;
	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
		keyClearSync (k);
}

void output_meta (Key * k)
{
	const Key * meta;

	keyRewindMeta (k);
	while ((meta = keyNextMeta (k)) != 0)
	{
		printf (", %s: %s", keyName (meta), (const char *)keyValue (meta));
	}
	printf ("\n");
}

void output_key (Key * k)
{
	// output_meta will print endline
	printf ("%p key: %s, string: %s", (void *)k, keyName (k), keyString (k));
	output_meta (k);
}

void output_keyset (KeySet * ks)
{
	Key * k;
	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
	{
		output_key (k);
	}
}

void output_plugin (Plugin * plugin)
{
	if (!plugin)
		return;

	printf ("Name: %s [%zd]\n", plugin->name, plugin->refcounter);
	output_keyset (plugin->config);
}

void output_backend (Backend * backend)
{
	if (!backend)
		return;

	printf ("us: %zd, ss: %zd\n", backend->usersize, backend->systemsize);
	output_key (backend->mountpoint);
}

void output_trie (Trie * trie)
{
	int i;
	for (i = 0; i < KDB_MAX_UCHAR; ++i)
	{
		if (trie->value[i])
		{
			printf ("output_trie: %p, mp: %s %s [%d]\n", (void *)trie->value[i], keyName (trie->value[i]->mountpoint),
				keyString (trie->value[i]->mountpoint), i);
		}
		if (trie->children[i])
			output_trie (trie->children[i]);
	}
	if (trie->empty_value)
	{
		printf ("empty_value: %p, mp: %s %s\n", (void *)trie->empty_value, keyName (trie->empty_value->mountpoint),
			keyString (trie->empty_value->mountpoint));
	}
}

void output_split (Split * split)
{
	printf ("Split - size: %zd, alloc: %zd\n", split->size, split->alloc);
	for (size_t i = 0; i < split->size; ++i)
	{
		if (split->handles[i])
		{
			printf ("split #%zd size: %zd, handle: %p, sync: %d, parent: %s (%s), spec: %zd, dir: %zd, user: %zd, system: "
				"%zd\n",
				i, ksGetSize (split->keysets[i]), (void *)split->handles[i], split->syncbits[i],
				keyName (split->parents[i]), keyString (split->parents[i]), split->handles[i]->specsize,
				split->handles[i]->dirsize, split->handles[i]->usersize, split->handles[i]->systemsize);
		}
		else
		{
			printf ("split #%zd, size: %zd, default split, sync: %d\n", i, ksGetSize (split->keysets[i]), split->syncbits[i]);
		}
	}
}

void generate_split (Split * split)
{
	printf ("succeed_if (split->size == %zd, \"size of split not correct\");\n", split->size);
	for (size_t i = 0; i < split->size; ++i)
	{
		printf ("succeed_if (split->syncbits[%zd]== %d, \"size of split not correct\");\n", i, split->syncbits[i]);
		printf ("succeed_if (ksGetSize(split->keysets[%zd]) == %zd, \"wrong size\");\n", i, ksGetSize (split->keysets[i]));
	}
}

/**
 * @brief Output warnings if present
 *
 * To check for warnings use:
 * succeed_if(output_warnings(parentKey), "warning(s) found");
 *
 * @param warningKey the key to retrieve metadata from
 *
 * @see check_for_errors_and_warnings if you want errors to have a test case failed without output
 *
 * @retval 1 if no warnings (can be used within succeed_if)
 */
int output_warnings (Key * warningKey)
{
	//! [warnings]
	const Key * metaWarnings = keyGetMeta (warningKey, "warnings");
	if (!metaWarnings)
		return 1; /* There are no current warnings */

	int nrWarnings = atoi (keyString (metaWarnings));
	char buffer[] = "warnings/#00\0description";

	printf ("There are %d warnings\n", nrWarnings + 1);
	for (int i = 0; i <= nrWarnings; ++i)
	{
		buffer[10] = i / 10 % 10 + '0';
		buffer[11] = i % 10 + '0';
		printf ("buffer is: %s\n", buffer);
		strncat (buffer, "/number", sizeof (buffer) - 1);
		printf ("number: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/description", sizeof (buffer) - 1);
		printf ("description: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/ingroup", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("ingroup: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/module", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("module: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/file", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("file: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/line", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("line: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/reason", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("reason: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/mountpoint", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("reason: %s\n", keyString (keyGetMeta (warningKey, buffer)));
		buffer[12] = '\0';
		strncat (buffer, "/configfile", sizeof (buffer) - 1);
		keyGetMeta (warningKey, buffer);
		printf ("reason: %s\n", keyString (keyGetMeta (warningKey, buffer)));
	}
	//! [warnings]

	return 0;
}

/**
 * @brief Output the error if present
 *
 * To check for error use:
 * succeed_if(output_error(parentKey), "error found");
 *
 * @param errorKey keys to retrieve errors from
 *
 * @retval 1 if no error (can be used within succeed_if)
 */
int output_error (Key * errorKey)
{
	//! [error]
	const Key * metaError = keyGetMeta (errorKey, "error");
	if (!metaError)
		return 1; /* There is no current error */

	printf ("number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
	printf ("description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
	printf ("ingroup: : %s\n", keyString (keyGetMeta (errorKey, "error/ingroup")));
	printf ("module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
	printf ("at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")), keyString (keyGetMeta (errorKey, "error/line")));
	printf ("reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
	printf ("mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
	printf ("configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
	//! [error]

	return 0;
}

static void clean_temp_home (void)
{
	if (tmpfilename)
	{
		elektraUnlink (tmpfilename);
		elektraFree (tmpfilename);
		tmpfilename = NULL;
	}

	if (tempHomeConf)
	{
		rmdir (tempHomeConf);
		elektraFree (tempHomeConf);
		tempHomeConf = NULL;
	}

	if (tempHome)
	{
		rmdir (tempHome);
		elektraFree (tempHome);
		tempHome = NULL;
		tempHomeLen = 0;
	}
}
