#include <tests.h>

#include <kdbinternal.h>

int nbError;
int nbTest;

uid_t nbUid;
gid_t nbGid;

char file [KDB_MAX_PATH_LENGTH];
char srcdir [KDB_MAX_PATH_LENGTH];

#ifdef HAVE_CLEARENV
int clearenv();
#endif

/**Does some useful startup.
 */
int init (int argc, char**argv)
{
	setlocale (LC_ALL, "");

	nbUid = getuid();
	nbGid = getgid();

	if (getenv ("srcdir"))
	{
		strncpy (srcdir, getenv ("srcdir"), sizeof(srcdir));
	} else {
		if (argc > 1)
		{
			strncpy (srcdir, argv[1], sizeof(srcdir));
		} else {
			strcpy (srcdir, ".");
			warn_if_fail (0, "srcdir not set, will try current directory");
		}
	}
#ifdef HAVE_CLEARENV
	clearenv();
#else
	unsetenv("HOME");
	unsetenv("USER");
	unsetenv("KDB_HOME");
	unsetenv("KDB_USER");
	unsetenv("KDB_DIR");
#endif

#ifdef HAVE_SETENV
	setenv("KDB_HOME",".",1);
#endif

	return 0;
}

/**Create a root key for a backend.
 *
 * @return a allocated root key */
Key * create_root_key (const char *backendName)
{
	Key *root = keyNew ("user/tests", KEY_END);
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
KeySet *create_conf (const char *filename)
{
	return ksNew (2,
		keyNew("system/path", KEY_VALUE, filename, KEY_END),
		KS_END);
}


int compare_line_files (const char *filename, const char *genfilename)
{
	FILE *forg, *fgen;
	char bufferorg [BUFFER_LENGTH + 1];
	char buffergen [BUFFER_LENGTH + 1];
	int line = 0;

	forg = fopen (filename, "r");
	fgen = fopen (genfilename, "r");
	exit_if_fail (forg && fgen, "could not open file");

	while (	fgets (bufferorg, BUFFER_LENGTH, forg) &&
		fgets (buffergen, BUFFER_LENGTH, fgen))
	{
		line ++;
		if (strncmp (bufferorg, buffergen, BUFFER_LENGTH))
		{
			printf ("In file %s, line %d.\n", filename, line);
			fclose (forg); fclose (fgen);
			succeed_if (0, "comparing lines failed");
			return 0;
		}
	}
	fclose (forg); fclose (fgen);
	return 1;
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
	char genfilename [KDB_MAX_PATH_LENGTH];
	char * dot = strrchr (filename, '.');

	exit_if_fail (dot != 0, "could not find extension in file");

	strncpy (genfilename, filename, dot- filename);
	/* does not terminate string, but strcat need it, so: */
	genfilename[dot-filename] = 0;
	strcat  (genfilename, "-gen");
	if (!strcmp (dot, ".xml"))
	{
		strcat (genfilename, ".xml");
	} else if (!strcmp (dot, ".txt"))
	{
		strcat (genfilename, ".txt");
	} else if (!strcmp (dot, ".c"))
	{
		strcat (genfilename, ".c");
	}

	return compare_line_files (filename, genfilename);
}

void check_attributes(keyswitch_t attributes)
{
	succeed_if ((attributes & KEY_NAME) == 0 , "compare key: NAME not equal");
	succeed_if ((attributes & KEY_VALUE) == 0 , "compare key: VALUE not equal");
	succeed_if ((attributes & KEY_OWNER) == 0 , "compare key: OWNER not equal");
	succeed_if ((attributes & KEY_COMMENT) == 0 , "compare key: COMMENT not equal");
	succeed_if ((attributes & KEY_UID) == 0 , "compare key: UID not equal");
	succeed_if ((attributes & KEY_GID) == 0 , "compare key: GID not equal");
	succeed_if ((attributes & KEY_MODE ) == 0 , "compare key: MODE  not equal");
	succeed_if ((attributes & KEY_NULL ) == 0, "compare key: one of the keys is null");
}


int compare_key (Key *k1, Key *k2)
{
	int	err = nbError;

	/* printf ("compare: "); keyOutput (k1, stdout); keyOutput (k2, stdout); printf ("\n"); */
	// printf ("compare value %s with %s\n", keyValue(k1), keyValue(k2));
	keyswitch_t attributes = keyCompare(k1, k2);
	check_attributes(attributes);


	const Key * meta;
	keyRewindMeta(k1);
	keyRewindMeta(k2);
	while ((meta = keyNextMeta (k1)) != 0)
	{
		const Key const * metaCmp = keyNextMeta(k2);
		if (metaCmp == 0)
		{
			succeed_if (0, "Will break, did not find corresponding metaCmp (k1 > k2)");
			break;
		}
		attributes = keyCompare(meta, metaCmp);
		check_attributes(attributes);
	}

	// check if there is a leftover key
	const Key const * metaCmp = keyNextMeta(k2);
	if (metaCmp != 0)
	{
		succeed_if (0, "There are too many metaCmp keys (k1 < k2)");
	}

	return err-nbError;
}

/**Compare two keysets.
 *
 * Compare if two keysets contain the same keys.
 * @return 0 on success
 * */
int compare_keyset (KeySet *ks, KeySet *ks2)
{
	Key	*key = 0;
	Key     *key2 = 0;
	int	err = nbError;

	if (ksGetSize (ks)  == 0) succeed_if (0, "real size of ks was 0");
	if (ksGetSize (ks2) == 0) succeed_if (0, "real size of ks2 was 0");

	if (ksGetSize (ks) != ksGetSize(ks2) ) {
		printf ("%d, %d\n", (int)ksGetSize(ks), (int)ksGetSize(ks2));
		succeed_if( 0, "Size of keysets not equal.");
	}

	// I would have a _true_ ksCompare() ...
	ksRewind(ks);
	ksRewind(ks2);

	//SYNC with ksOutput
	while ((key = ksNext(ks)) != 0)
	{
		key2 = ksNext(ks2);
		if (!key2)
		{
			succeed_if (0, "Will break, did not find corresponding key2");
			break;
		}

		compare_key (key, key2);
	}
	return err-nbError;
}

/* return file name in srcdir.
 * No bound checking on file size, may overflow. */
char * srcdir_file(const char * fileName)
{
	strcpy(file, srcdir);
	strcat(file, "/");
	strcat(file, fileName);
	return file;
}

void clear_sync (KeySet *ks)
{
	Key *k;
	ksRewind(ks);
	while ((k = ksNext(ks)) != 0) keyClearSync(k);
}

void output_key (Key *k)
{
	printf ("key: %s, string: %s\n", keyName(k), keyString(k));
}

void output_keyset (KeySet *ks)
{
	Key *k;
	ksRewind(ks);
	while ((k = ksNext(ks)) != 0)
	{
		printf ("key: %s, string: %s\n", keyName(k), keyString(k));
	}
}

void output_plugin(Plugin *plugin)
{
	if (!plugin) return;

	printf ("Name: %s [%zd]\n", plugin->name, plugin->refcounter);
	output_keyset(plugin->config);
}

void output_backend(Backend *backend)
{
	if (!backend) return;

	printf ("us: %zd, ss: %zd\n", backend->usersize, backend->systemsize);
	output_key (backend->mountpoint);
}

void output_trie(Trie *trie)
{
	int i;
	for (i=0; i <= KDB_MAX_UCHAR; ++i)
	{
		if (trie->value[i])
		{
			printf ("output_trie: %p, mp: %s %s [%d]\n",
					(void*) trie->value[i],
					keyName(trie->value[i]->mountpoint),
					keyString(trie->value[i]->mountpoint),
					i);
		}
		if (trie->children[i]) output_trie(trie->children[i]);
	}
	if (trie->empty_value)
	{
		printf ("empty_value: %p, mp: %s %s\n",
				(void*) trie->empty_value,
				keyName(trie->empty_value->mountpoint),
				keyString(trie->empty_value->mountpoint)
				);
	}
}

void output_split(Split *split)
{
	for (size_t i=0; i<split->size; ++i)
	{
		if (split->handles[i])
		{
			printf ("split #%zd size: %zd, handle: %p, sync: %d, parent: %s (%s), us: %zd, ss: %zd\n",
				i,
				ksGetSize(split->keysets[i]),
				(void*)split->handles[i],
				split->syncbits[i],
				keyName(split->parents[i]),
				keyString(split->parents[i]),
				split->handles[i]->usersize,
				split->handles[i]->systemsize
				);
		} else {
			printf ("split #%zd, size: %zd, default split, sync: %d\n",
				i,
				ksGetSize(split->keysets[i]),
				split->syncbits[i]
				);
		}
	}
}

void generate_split (Split *split)
{
	printf ("succeed_if (split->size == %zd, \"size of split not correct\");\n", split->size);
	for (size_t i=0; i<split->size; ++i)
	{
		printf ("succeed_if (split->syncbits[%zd]== %d, \"size of split not correct\");\n", i, split->syncbits[i]);
		printf ("succeed_if (ksGetSize(split->keysets[%zd]) == %zd, \"wrong size\");\n", i, ksGetSize(split->keysets[i]));
	}

}

void output_warnings(Key *warningKey)
{
	const Key *metaWarnings = keyGetMeta(warningKey, "warnings");
	if (!metaWarnings) return; /* There are no current warnings */
	succeed_if (0, "there were warnings issued");

	int nrWarnings = atoi(keyString(metaWarnings));
	char buffer[] = "warnings/#00\0description";

	printf ("There are %d warnings\n", nrWarnings+1);
	for (int i=0; i<=nrWarnings; ++i)
	{
		buffer[10] = i/10%10 + '0';
		buffer[11] = i%10 + '0';
		printf ("buffer is: %s\n", buffer);
		strncat(buffer, "/number" , sizeof(buffer));
		printf ("number: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/description" , sizeof(buffer));
		printf ("description: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/ingroup" , sizeof(buffer));
		keyGetMeta(warningKey, buffer);
		printf ("ingroup: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/module" , sizeof(buffer));
		keyGetMeta(warningKey, buffer);
		printf ("module: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/file" , sizeof(buffer));
		keyGetMeta(warningKey, buffer);
		printf ("file: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/line" , sizeof(buffer));
		keyGetMeta(warningKey, buffer);
		printf ("line: %s\n", keyString(keyGetMeta(warningKey, buffer)));
		buffer[12] = '\0'; strncat(buffer, "/reason" , sizeof(buffer));
		keyGetMeta(warningKey, buffer);
		printf ("reason: %s\n", keyString(keyGetMeta(warningKey, buffer)));
	}
}

void output_errors(Key *errorKey)
{
	const Key * metaError = keyGetMeta(errorKey, "error");
	if (!metaError) return; /* There is no current error */
	succeed_if (0, "there were errors issued");

	printf ("number: %s\n", keyString(keyGetMeta(errorKey, "error/number")));
	printf ("description: : %s\n", keyString(keyGetMeta(errorKey, "error/description")));
	printf ("ingroup: : %s\n", keyString(keyGetMeta(errorKey, "error/ingroup")));
	printf ("module: : %s\n", keyString(keyGetMeta(errorKey, "error/module")));
	printf ("at: %s:%s\n", keyString(keyGetMeta(errorKey,"error/file")), keyString(keyGetMeta(errorKey, "error/line")));
	printf ("reason: : %s\n", keyString(keyGetMeta(errorKey, "error/reason")));
}
