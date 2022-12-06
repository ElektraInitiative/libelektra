/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#ifdef USE_NFTW
#include <ftw.h>
#include <stdlib.h>
#define NOPENFD 20
#endif

#include <regex.h>

#include <kdbinternal.h>
#include <elektra/kdbprivate.h>

#ifdef __cplusplus
extern "C" {
#endif

int nbError;
int nbTest;

char file[KDB_MAX_PATH_LENGTH];
char srcdir[KDB_MAX_PATH_LENGTH + 1];
char bindir[KDB_MAX_PATH_LENGTH + 1];

char * tmpfilename;
char * tempHome;
int tempHomeLen;
char * tempHomeConf;

/**Does some useful startup.
 */
int init (int argc, char ** argv)
{
	char * tmpvar;
	int fd;

	if (argc > 1)
	{
		strncpy (srcdir, argv[1], sizeof (srcdir) - 1);
	}
	else
	{
		strncpy (srcdir, BUILTIN_DATA_FOLDER, sizeof (srcdir) - 1);
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

	const char * binpath = getenv ("KDB_TEST_BIN_DIR");
	if (binpath != NULL)
	{
		strncpy (bindir, binpath, sizeof (bindir) - 1);
	}
	else
	{
		strncpy (bindir, BUILTIN_EXEC_FOLDER, sizeof (bindir) - 1);
	}

	return 0;
}

/**Create a root key for a backend.
 *
 * @return an allocated root key */
Key * create_root_key (const char * backendName)
{
	Key * root = keyNew ("user:/tests", KEY_END);
	/*Make mountpoint beneath root, and do all tests here*/
	keyAddBaseName (root, backendName);
	keySetString (root, backendName);
	keySetString (root, backendName);
	return root;
}

/**Create a configuration keyset for a backend.
 *
 * @return an allocated configuration keyset for a backend*/
KeySet * create_conf (const char * filename)
{
	return ksNew (2, keyNew ("system:/path", KEY_VALUE, filename, KEY_END), KS_END);
}


int compare_line_files_fun (const char * filename, const char * genfilename, int (*cmpFun) (const char *, const char *, size_t n))
{
	FILE *forg, *fgen;
	char bufferorg[BUFFER_LENGTH + 1];
	char buffergen[BUFFER_LENGTH + 1];
	char * org = 0;
	char * gen = 0;
	int line = 0;
	int remainingBufferLength = BUFFER_LENGTH;

	forg = fopen (filename, "r");
	fgen = fopen (genfilename, "r");

	strncpy (bufferorg, "could not open file, orig: ", BUFFER_LENGTH);
	remainingBufferLength -= strlen ("could not open file, orig: ");
	strncat (bufferorg, filename, remainingBufferLength);
	remainingBufferLength -= strlen (filename);
	strncat (bufferorg, " gen: ", remainingBufferLength);
	remainingBufferLength -= strlen (" gen: ");
	strncat (bufferorg, genfilename, remainingBufferLength);

	exit_if_fail (forg && fgen, bufferorg);

	while ((org = fgets (bufferorg, BUFFER_LENGTH, forg)) && (gen = fgets (buffergen, BUFFER_LENGTH, fgen)))
	{
		line++;
		if ((*cmpFun) (bufferorg, buffergen, BUFFER_LENGTH))
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
	return compare_line_files_fun (filename, genfilename, &strncmp);
}


/**
 * @brief Compare regex in pattern to str
 *
 * @param pattern char * representing a regex pattern
 * @param str char * we compare the pattern to
 *
 * @retval 1 if pattern is invalid or does not match str
 * @retval 0 on success
 */
int regexcmp (const char * pattern, const char * str, size_t n ELEKTRA_UNUSED)
{
	int status;
	regex_t re;

	if (regcomp (&re, pattern, REG_EXTENDED | REG_NOSUB) != 0)
	{
		return (1);
	}
	status = regexec (&re, str, (size_t) 0, NULL, 0);
	regfree (&re);
	return status;
}

/**
 * @brief Compare two files line by line where the original file is made up of
 *        regex
 *
 * @param filename first file, containing regex patterns
 * @param genfilename file to compare with
 *
 * @retval 0 on errors (succeed_if already executed)
 * @retval 1 on success
 */
int compare_regex_to_line_files (const char * filename, const char * genfilename)
{
	return compare_line_files_fun (filename, genfilename, &regexcmp);
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

/* return file name in srcdir.
 * No bound checking on file size, may overflow. */
char * bindir_file (const char * fileName)
{
	strcpy (file, bindir);
	strcat (file, "/");
	strcat (file, fileName);
	return file;
}

const char * elektraFilename (void)
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
	KeySet * metaKeys = keyMeta (k);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		const Key * meta = ksAtCursor (metaKeys, it);
		printf (", %s: %s", keyName (meta), (const char *) keyValue (meta));
	}
	printf ("\n");
}

void output_key (Key * k)
{
	// output_meta will print endline
	printf ("%p key: %s, string: %s", (void *) k, keyName (k), keyString (k));
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
	if (!plugin) return;

	printf ("Name: %s [%zu]\n", plugin->name, plugin->refcounter);
	output_keyset (plugin->config);
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
	Key * cutpoint = keyNew ("meta:/warnings", KEY_END);
	KeySet * warnings = ksCut (keyMeta (warningKey), cutpoint);

	if (!warningKey || ksGetSize (warnings) == 0)
	{
		ksDel (warnings);
		keyDel (cutpoint);
		return 1;
	}

	printf ("There are %zu warnings\n", ksGetSize (warnings));
	elektraCursor i = 1;
	while (i < ksGetSize (warnings))
	{
		++i;
		Key * cur = ksAtCursor (warnings, i);
		while (!keyIsDirectlyBelow (cutpoint, cur))
		{
			printf ("%s: %s\n", keyName (cur) + keyGetNameSize (cutpoint), keyString (cur));
			++i;
			cur = ksAtCursor (warnings, i);
		}
		printf ("\n");
	}
	ksDel (warnings);
	keyDel (cutpoint);
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
	if (!metaError) return 1; /* There is no current error */

	printf ("number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
	printf ("description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
	printf ("module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
	printf ("at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")), keyString (keyGetMeta (errorKey, "error/line")));
	printf ("reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
	printf ("mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
	printf ("configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
	//! [error]

	return 0;
}

#ifdef USE_NFTW
static int rm_all (const char * fpath, const struct stat * sb ELEKTRA_UNUSED, int tflag, struct FTW * ftwbuf ELEKTRA_UNUSED)
{
	if (tflag == FTW_F)
	{
		unlink (fpath);
	}
	else if (tflag == FTW_D || tflag == FTW_DP)
	{
		rmdir (fpath);
	}
	else
	{
		// not a file or dir we can delete
		printf ("unexpected flag: %d\n", tflag);
		return 1;
	}
	return 0;
}
#endif

void clean_temp_home (void)
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
#ifdef USE_NFTW
		int nftw_flags = FTW_DEPTH | FTW_PHYS;
		succeed_if (nftw (tempHome, rm_all, NOPENFD, nftw_flags) == 0, "Could not delete TMPHOME via nftw");
#else
		size_t fileToCleanLen = tempHomeLen + 30;
		char * fileToClean = elektraMalloc (fileToCleanLen);
		snprintf (fileToClean, fileToCleanLen, "%s/.gnupg/random_seed", tempHome);
		unlink (fileToClean);
		snprintf (fileToClean, fileToCleanLen, "%s/.gnupg/trustdb.gpg", tempHome);
		unlink (fileToClean);
		snprintf (fileToClean, fileToCleanLen, "%s/.gnupg/pubring.kbx~", tempHome);
		unlink (fileToClean);
		snprintf (fileToClean, fileToCleanLen, "%s/.gnupg/pubring.kbx", tempHome);
		unlink (fileToClean);
		snprintf (fileToClean, fileToCleanLen, "%s/.gnupg", tempHome);
		rmdir (fileToClean);
		elektraFree (fileToClean);

		succeed_if (rmdir (tempHome) == 0, "Could not delete TMPHOME manually");
#endif
		elektraFree (tempHome);
		tempHome = NULL;
		tempHomeLen = 0;
	}
}

#ifdef __cplusplus
} // end extern "C"
#endif
