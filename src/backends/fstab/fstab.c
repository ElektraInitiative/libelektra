/***************************************************************************
            temaple.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is a backend that takes /etc/fstab file as its backend storage.  *
 *   The kdbGetKeyChildKeys() method will parse /etc/fstab and generate a  *
 *   valid key tree. The kdbSetKeys() method will take a KeySet with valid *
 *   filesystem keys and print an equivalent regular fstab in stdout.      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id:$
$LastChangedBy: aviram $

*/



#include <kdb.h>
#include <kdbbackend.h>
#include <mntent.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>

#define BACKENDNAME "fstab"
#define ROOT        "system/filesystems"



/**Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posixsystem*/
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif



int kdbOpen_fstab() {
	/* backend initialization logic */
	return 0;
}




int kdbClose_fstab() {
	/* free all backend resources and shutdown */
	return 0; /* success */
}




int kdbGetKeyChildKeys_fstab(const Key *parentKey, KeySet *returned,
		unsigned long options) {
	FILE *fstab=0;
	struct mntent *fstabEntry;
	unsigned swapIndex=0;
	
	if (strcmp(parentKey->key,ROOT)) return -1;
	
	fstab=setmntent("/etc/fstab", "r");
	if (fstab == 0) return -1; /* propagate errno */
	
	while ((fstabEntry=getmntent(fstab))) {
		char fsname[MAX_PATH_LENGTH];
		char fsKeyName[MAX_PATH_LENGTH];
		Key *key=0;
		
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
			
			while((slash=index(curr,'/'))) {
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
		sprintf(fsKeyName,"%s/%s",ROOT,fsname);
		if (options & KDB_O_DIR || options & KDB_O_NOVALUE) {
			ksAppend(returned,key=keyNew(fsKeyName,
				KEY_SWITCH_TYPE,KEY_TYPE_DIR,
				KEY_SWITCH_COMMENT,"Filesystem pseudo-name",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END));
			key->flags &= ~KEY_SWITCH_NEEDSYNC;
		}
			
		
		/* Include filesystems infos */
		if (options & KDB_O_RECURSIVE) {
			char buffer[MAX_PATH_LENGTH];
			
			sprintf(buffer,"%s/%s",fsKeyName,"device");
			ksAppend(returned, key=keyNew(buffer,
				KEY_SWITCH_VALUE,fstabEntry->mnt_fsname,
				KEY_SWITCH_COMMENT,"Device or Label",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END));
			key->flags &= ~KEY_SWITCH_NEEDSYNC;

			
			sprintf(buffer,"%s/%s",fsKeyName,"mpoint");
			ksAppend(returned, key=keyNew(buffer,
				KEY_SWITCH_VALUE,fstabEntry->mnt_dir,
				KEY_SWITCH_COMMENT,"Moint point",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END));
			key->flags &= ~KEY_SWITCH_NEEDSYNC;

			
			sprintf(buffer,"%s/%s",fsKeyName,"type");
			ksAppend(returned, key=keyNew(buffer,
				KEY_SWITCH_VALUE,fstabEntry->mnt_type,
				KEY_SWITCH_COMMENT,"Filesystem type. See fs(5)",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END));
			key->flags &= ~KEY_SWITCH_NEEDSYNC;
			
			
			sprintf(buffer,"%s/%s",fsKeyName,"options");
			ksAppend(returned, key=keyNew(buffer,
				KEY_SWITCH_VALUE,fstabEntry->mnt_opts,
				KEY_SWITCH_COMMENT,"Filesystem specific options. See mount(8)",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END));
			key->flags &= ~KEY_SWITCH_NEEDSYNC;
			
			
			sprintf(buffer,"%d",fstabEntry->mnt_freq);
			key=keyNew(ROOT,
				KEY_SWITCH_VALUE,buffer,
				KEY_SWITCH_COMMENT,"Dump frequency in days",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END);
			sprintf(buffer,"%s/%s",fsKeyName,"dumpfreq");
			keySetName(key,buffer);
			key->flags &= ~KEY_SWITCH_NEEDSYNC;
			ksAppend(returned, key);
			
			
			sprintf(buffer,"%d",fstabEntry->mnt_passno);
			key=keyNew(ROOT,
				KEY_SWITCH_VALUE,buffer,
				KEY_SWITCH_COMMENT,"Pass number on parallel fsck",
				KEY_SWITCH_UID,0,
				KEY_SWITCH_GID,0,
				KEY_SWITCH_END);
			sprintf(buffer,"%s/%s",fsKeyName,"passno");
			keySetName(key,buffer);
			key->flags &= ~KEY_SWITCH_NEEDSYNC;
			ksAppend(returned, key);
		}
	}
	
	endmntent(fstab);
	
	if ((options & (KDB_O_SORT)) && (ksGetSize(returned) > 1))
		ksSort(returned);

	return 0; /* success */
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbSetKey() for each
 * key inside @p ks.
 *
 * @see kdbSetKeys() for expected behavior.
 * @ingroup backend
 */
int kdbSetKeys_fstab(KeySet *ks) {
	regex_t regex;
	u_int32_t match=0;
	struct mntent fstabEntry;
	Key *key=0;
	
	while (1) {
		/* lookup for the first entire filesystem keys */
		regcomp(&regex,"system/filesystems/[^/]*$",REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,0);
		regfree(&regex);
	
		if (match == 0) return 0;
	
		/* ksNext(ks); */
		regcomp(&regex,"system/filesystems/[^/]*/device$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_fsname=(char *)key->data;
		}
		regfree(&regex);
	
		regcomp(&regex,"system/filesystems/[^/]*/dumpfreq$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_freq=atoi((char *)key->data);
		}
		regfree(&regex);
	
		regcomp(&regex,"system/filesystems/[^/]*/mpoint$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_dir=(char *)key->data;
		}
		regfree(&regex);
	
		regcomp(&regex,"system/filesystems/[^/]*/options$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_opts=(char *)key->data;
		}
		regfree(&regex);
	
		regcomp(&regex,"system/filesystems/[^/]*/passno$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_passno=atoi((char *)key->data);
		}
		regfree(&regex);
	
		regcomp(&regex,"system/filesystems/[^/]*/type$",REG_ICASE | REG_NOSUB);
		match=ksLookupRE(ks,KEY_SWITCH_NAME,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			fstabEntry.mnt_type=(char *)key->data;
		}
		regfree(&regex);
	
	
		printf("%s   %s   %s   %s   %d %d\n",
			fstabEntry.mnt_fsname,
			fstabEntry.mnt_dir,
			fstabEntry.mnt_type,
			fstabEntry.mnt_opts,
			fstabEntry.mnt_freq,
			fstabEntry.mnt_passno);
	}

	return 0;
}




KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_fstab,
		KDB_BE_CLOSE,          &kdbClose_fstab,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_fstab,
		KDB_BE_SETKEYS,        &kdbSetKeys_fstab,
		KDB_BE_END);
}
