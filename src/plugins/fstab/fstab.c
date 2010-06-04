/***************************************************************************
            fstab.c  -  Access the /etc/fstab file
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is a backend that takes /etc/fstab file as its backend storage.  *
 *   The kdbGet() method will parse /etc/fstab and generate a              *
 *   valid key tree. The kdbSet() method will take a KeySet with valid     *
 *   filesystem keys and print an equivalent regular fstab in stdout.      *
 *                                                                         *
 ***************************************************************************/

#include "fstab.h"

int kdbOpen_fstab(KDB *handle)
{
	KDBCap * cap = kdbhGetCapability(handle);
	const void *f;
	KeySet *ks;
	Key *k;

	cap->onlyFullGet=1;
	cap->noStat=1;

	cap->onlyRemoveAll=1;
	cap->onlyAddKeys=1;
	cap->onlyFullSet=1;

	cap->noComment=1;
	cap->noUID=1;
	cap->noGID=1;
	cap->noMode=1;
	cap->noATime=1;
	cap->noMTime=1;
	cap->noCTime=1;
	cap->noRemove=1;
	cap->noMount=1;
	cap->noBinary=1;
	cap->noTypes=1;
	cap->noError=1;

	cap->noLock = 1;
	cap->noThread = 1;

	ks = kdbhGetConfig (handle);
	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
	{
		f = keyName (k);
		if (f) f = strrchr (f, '/');
		if (f && strcmp (f, "/path") == 0) {
			void *data=malloc(keyGetValueSize(k));
			keyGetString (k, data, keyGetValueSize(k));
			kdbhSetBackendData (handle, data);
		}
	}
	if (!kdbhGetBackendData (handle)) kdbhSetBackendData (handle, elektraStrDup (FSTAB_PATH));
	/* backend initialization logic */
#if DEBUG && VERBOSE
	printf ("open fstab backend with %s\n", kdbhGetBackendData (handle));
#endif
	return 0;
}




int kdbClose_fstab(KDB *handle)
{
	/* free all backend resources and shutdown */
	free(kdbhGetBackendData(handle));
#if DEBUG && VERBOSE
	printf ("close fstab backend\n");
#endif
	return 0; /* success */
}

#define MAX_NUMBER_SIZE 10

ssize_t kdbGet_fstab(KDB *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0;
	Key *key;
	Key *dir;
	FILE *fstab=0;
	struct mntent *fstabEntry;
	char fsname[MAX_PATH_LENGTH];
	char buffer[MAX_NUMBER_SIZE];
	const char *mountpointname = keyName(kdbhGetMountpoint(handle));
	const char *parentname = keyName(parentKey);

#if DEBUG && VERBOSE
	printf ("get fstab %s, point: %s\n", keyName(parentKey), mountpointname);
#endif

	if (strcmp (mountpointname, parentname)) return 0;

	ksClear (returned);
	key = keyDup (parentKey);
	keySetDir(key);
	ksAppendKey(returned, key);
	nr_keys ++;
	key->flags &= ~KEY_FLAG_SYNC;

	fstab=setmntent(kdbhGetBackendData(handle), "r");
	if (fstab == 0)
	{
		/* propagate errno */
		errno = errnosave;
		return -1;
	}
	
	while ((fstabEntry=getmntent(fstab)))
	{
		unsigned int swapIndex=0;
		nr_keys += 7;

		/* Some logic to define the filesystem name when it is not
		 * so obvious */
		if (!strcmp(fstabEntry->mnt_type,"swap")) {
			sprintf(fsname,"swap%02d",swapIndex);
			swapIndex++;
		} else if (!strcmp(fstabEntry->mnt_dir,"none")) {
			strcpy(fsname,fstabEntry->mnt_type);
		} else if (!strcmp(fstabEntry->mnt_dir,"/")) {
			strcpy(fsname,"rootfs");
		} else {
			/* fsname will be the mount point without '/' char */
			char *slash=0;
			char *curr=fstabEntry->mnt_dir;
			fsname[0]=0;
			
			while((slash=strchr(curr,PATH_SEPARATOR))) {
				if (slash==curr) {
					curr++;
					continue;
				}
				
				strncat(fsname,curr,slash-curr);
				curr=slash+1;
			}
			strcat(fsname,curr);
		}
		
		/* Include only the filesystem pseudo-names */
		dir = keyDup (parentKey);
		keyAddBaseName(dir, fsname);
		keySetString(dir,"");
		keySetComment(dir,"");
		keySetMode(dir, 0664); /* TODO stat */
		keySetComment (dir, "Filesystem pseudo-name");
		ksAppendKey(returned,dir);

		key = keyDup (dir);
		keyAddBaseName(key, "device");
		keySetString (key, fstabEntry->mnt_fsname);
		keySetComment (key, "Device or Label");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		key = keyDup (dir);
		keyAddBaseName(key, "mpoint");
		keySetString (key, fstabEntry->mnt_dir);
		keySetComment (key, "Mount point");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		key = keyDup (dir);
		keyAddBaseName(key, "type");
		keySetString (key, fstabEntry->mnt_type);
		keySetComment (key, "Filesystem type.");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		key = keyDup (dir);
		keyAddBaseName(key, "options");
		keySetString (key, fstabEntry->mnt_opts);
		keySetComment (key, "Filesystem specific options");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		key = keyDup (dir);
		keyAddBaseName(key, "dumpfreq");
		snprintf(buffer, MAX_NUMBER_SIZE, "%d",fstabEntry->mnt_freq);
		keySetString (key, buffer);
		keySetComment (key, "Dump frequency in days");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		key = keyDup (dir);
		keyAddBaseName(key, "passno");
		snprintf(buffer, MAX_NUMBER_SIZE, "%d",fstabEntry->mnt_passno);
		keySetString (key, buffer);
		keySetComment (key, "Pass number on parallel fsck");
		ksAppendKey(returned, key);
		key->flags &= ~KEY_FLAG_SYNC;

		keySetDir (dir);
		dir->flags &= ~KEY_FLAG_SYNC;
	}
	
	endmntent(fstab);

	errno = errnosave;
	return nr_keys;
}


ssize_t kdbSet_fstab(KDB *handle, KeySet *ks, const Key *parentKey)
{
	int ret = 1;
	int errnosave = errno;
	FILE *fstab=0;
	Key *key=0;
	char *basename = 0;
	const void *rootname = 0;
	struct mntent fstabEntry;
	const char *mountpointname = keyName(kdbhGetMountpoint(handle));
	const char *parentname = keyName(parentKey);

#if DEBUG && VERBOSE
	printf ("set fstab %s, point: %s\n", keyName(parentKey), mountpointname);
#endif

	if (strcmp (mountpointname, parentname)) return 0;

	ksRewind (ks);
	if ((key = ksNext (ks)) != 0)
	{
		unlink(kdbhGetBackendData(handle));
		return ret;
	} /*skip parent key*/

	fstab=setmntent(kdbhGetBackendData(handle), "w");
	memset(&fstabEntry,0,sizeof(struct mntent));

	while ((key = ksNext (ks)) != 0)
	{
		ret ++;
		basename=strrchr(keyName(key), '/')+1;
#if DEBUG && VERBOSE
		printf ("key: %s %s\n", keyName(key), basename);
#endif
		if (!strcmp (basename, "device"))
		{
			fstabEntry.mnt_fsname=(char *)keyValue(key);
		} else if (!strcmp (basename, "mpoint")) {
			fstabEntry.mnt_dir=(char *)keyValue(key);
		} else if (!strcmp (basename, "type")) {
			fstabEntry.mnt_type=(char *)keyValue(key);
		} else if (!strcmp (basename, "options")) {
			fstabEntry.mnt_opts=(char *)keyValue(key);
		} else if (!strcmp (basename, "dumpfreq")) {
			fstabEntry.mnt_freq=atoi((char *)keyValue(key));
		} else if (!strcmp (basename, "passno")) {
			fstabEntry.mnt_passno=atoi((char *)keyValue(key));
		} else { // new rootname
			if (!rootname)
			{
				rootname = keyValue(key);
			} else {
				rootname = keyValue(key);
#if DEBUG && VERBOSE
				fprintf(stdout, "first: %s   %s   %s   %s   %d %d\n",
					fstabEntry.mnt_fsname,
					fstabEntry.mnt_dir,
					fstabEntry.mnt_type,
					fstabEntry.mnt_opts,
					fstabEntry.mnt_freq,
					fstabEntry.mnt_passno);
#endif
				addmntent(fstab, &fstabEntry);
				memset(&fstabEntry,0,sizeof(struct mntent));
			}
		}
	}

	if (rootname)
	{
#if DEBUG && VERBOSE
		fprintf(stdout, "last: %s   %s   %s   %s   %d %d\n",
			fstabEntry.mnt_fsname,
			fstabEntry.mnt_dir,
			fstabEntry.mnt_type,
			fstabEntry.mnt_opts,
			fstabEntry.mnt_freq,
			fstabEntry.mnt_passno);
#endif
		addmntent(fstab, &fstabEntry);
	}
	
	endmntent(fstab);
	errno = errnosave;
	return ret;
}


KDB *ELEKTRA_PLUGIN_EXPORT(fstab) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_fstab,
		KDB_BE_CLOSE,          &kdbClose_fstab,
		KDB_BE_GET,            &kdbGet_fstab,
		KDB_BE_SET,            &kdbSet_fstab,
		KDB_BE_VERSION,        BACKENDVERSION,
		KDB_BE_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"Reads and writes /etc/fstab content",
		KDB_BE_END);
}


