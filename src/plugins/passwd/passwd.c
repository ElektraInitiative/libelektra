/***************************************************************************
            passwd.c  -  Access the /etc/passwd file
                             -------------------
    begin                : Nov 15 2007
    copyright            : (C) 2007 by Patrick Sabin
    email                : patricksabin@gmx.at
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include "passwd.h"

int kdbOpen_passwd(KDB *handle)
{
	KeySet *ks;
	Key *key;
	const char *s, *t;
	int prefix_len;
	PasswdData *data;
	const Key* mnt = kdbhGetMountpoint(handle);
	KDBCap *cap = kdbhGetCapability (handle);
	size_t mntsize = keyGetNameSize(mnt)+1;

	if (mnt==0)
	{
		return -1;
	}

	data=malloc(sizeof(PasswdData));
	kdbhSetBackendData(handle,data);
	data->mountpoint=0;
	ks=kdbhGetConfig(handle);

	ksRewind(ks);
	key=ksNext(ks);

	s=keyName(key);
	t=s+KDB_KEY_MOUNTPOINTS_LEN;

	/* skip entry name */
	while (!(*t=='/'||!*t))
		t++;
	
	if (!*t) {
		fprintf(stderr, "Cannot happen\n");
		exit(1);
	}

	
	t+=sizeof("/config/")-1;
	prefix_len=t-s;

	data->mountpoint=malloc(mntsize);
	snprintf(data->mountpoint,mntsize,"%s/",keyName(mnt));


	data->path="/etc/passwd";
	data->backend=BACKENDNAME;
	for (;key;key=ksNext(ks)) {
		if (strlen(keyName(key))<prefix_len) continue;

		if (!strncmp(keyName(key)+prefix_len,"path",sizeof("path"))) {
			data->path=keyValue(key);
		}
	}

	data->mountpointlen=strlen(data->mountpoint);

	cap->onlyRemoveAll=1;
	cap->onlyAddKeys=1;
	cap->onlyFullSet=1;

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
	cap->noStat=1;
	cap->noMount=1;
	cap->noBinary=1;
	cap->noString=1;
	cap->noTypes=1;
	cap->noError=1;

	return 0;
}




/**
 * Finalize the backend.
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 *
 * Make sure to free all memory that your backend requested at runtime.
 *
 * After this call, libelektra.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_passwd(KDB *handle)
{
	/* free all backend resources and shut it down */
	PasswdData *data;
	data=kdbhGetBackendData(handle);
	free(data->mountpoint);
	free(data);

	return 0; /* success */
}



/**
 * Implementation for kdbGet() method.
 *
 * @see kdbGet() for caller.
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to get the keys
 * @return the number of keys got
 * @return 0 on success with no changed key
 * @return -1 on failure, the current key in returned shows the position and/or errno is set
 * @ingroup backend
 */
ssize_t kdbGet_passwd(KDB *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0;
	/* get all keys below parentKey and count them with nr_keys */
	PasswdData *data;
	const char *parent;
	int parent_len;
	/*KeySet *ks;
	Key *key;
	char *s,*t;
	int prefix_len;
	*/
	data=kdbhGetBackendData(handle);
	parent=keyName(parentKey);
	parent_len=strlen(parent);

	if (parent_len<data->mountpointlen-1 ||
			strncmp(data->mountpoint,parent,data->mountpointlen-1))
	{
		// parentkey covers keys not handled by the backend
		return -1;
	}

	if (parent_len==data->mountpointlen-1 &&
			!strncmp(data->mountpoint, parent,data->mountpointlen-1))
	{
		// return all username directories
		char *name;
		FILE *f;
		struct passwd *pwd;

		f=fopen(data->path,"r");

		if (f==NULL) {
			/*errno=KDB_ERR_BACKEND;*/
			errno = errnosave;
			return -1;
		}
		while ((pwd=fgetpwent(f))) {
			char *pw_name;
			int pw_name_len;
			pw_name=pwd->pw_name;
			pw_name_len=strlen(pw_name);

			name=malloc(data->mountpointlen+pw_name_len+1);
			strncpy(name,data->mountpoint,data->mountpointlen+1);
			strncat(name,pw_name,pw_name_len);
			ksAppendKey(returned, keyNew(name,KEY_VALUE,"",0));
			nr_keys++;
			free (name);
		}

		fclose(f);

	} else if (parent_len>data->mountpointlen &&
			!strncmp(data->mountpoint,parent,data->mountpointlen))
	{
		const char *username, *p;
		p=username=parent+data->mountpointlen;
		while (!(*p=='/'||*p==0)) p++;
		if (*p==0) {
			FILE *f;
			struct passwd *pwd;
			int usernamelen;

			usernamelen=p-username;
			username=strndup(username,usernamelen);

			/* get username directory */
			f=fopen(data->path,"r");
			if (!f)
			{
				errno = errnosave;
				return -1;
			}
			while ((pwd=fgetpwent(f))) {
				if (!strncmp(pwd->pw_name,username,usernamelen)) {
					char *name;
					char *id;
					size_t id_size;

					name=malloc(data->mountpointlen+usernamelen+sizeof("/gecos"));
					strncpy(name,data->mountpoint,data->mountpointlen+1);
					strncat(name,username,usernamelen);
					strncat(name,"/gecos",sizeof("/gecos")-1);
					ksAppendKey(returned, keyNew(name,KEY_VALUE,pwd->pw_gecos,0));
					nr_keys++;
					free (name);

					name=malloc(data->mountpointlen+usernamelen+sizeof("/home"));
					strncpy(name,data->mountpoint,data->mountpointlen+1);
					strncat(name,username,usernamelen);
					strncat(name,"/home",sizeof("/home")-1);
					ksAppendKey(returned, keyNew(name,KEY_VALUE,pwd->pw_dir,0));
					nr_keys++;
					free (name);

					name=malloc(data->mountpointlen+usernamelen+sizeof("/shell"));
					strncpy(name,data->mountpoint,data->mountpointlen+1);
					strncat(name,username,usernamelen);
					strncat(name,"/shell",sizeof("/shell")-1);
					ksAppendKey(returned, keyNew(name,KEY_VALUE,pwd->pw_shell,0));
					nr_keys++;
					free (name);

					name=malloc(data->mountpointlen+usernamelen+sizeof("/uid"));
					strncpy(name,data->mountpoint,data->mountpointlen+1);
					strncat(name,username,usernamelen);
					strncat(name,"/uid",sizeof("/uid")-1);
					id_size=snprintf(NULL,0,"%d",(int)pwd->pw_uid);
					id=malloc(id_size);
					snprintf(id,id_size,"%d",(int)pwd->pw_uid);
					ksAppendKey(returned, keyNew(name,KEY_VALUE,id,0));
					nr_keys++;
					free (name);
					free (id);

					name=malloc(data->mountpointlen+usernamelen+sizeof("/gid"));
					strncpy(name,data->mountpoint,data->mountpointlen+1);
					strncat(name,username,usernamelen);
					strncat(name,"/gid",sizeof("/gid")-1);
					id_size=snprintf(NULL,0,"%d",(int)pwd->pw_gid);
					id=malloc(id_size);
					snprintf(id,id_size,"%d",(int)pwd->pw_gid);
					ksAppendKey(returned, keyNew(name,KEY_VALUE,id,0));
					nr_keys++;
					free (name);
					free (id);

					break;
				}
			}
			fclose (f);
			elektraFree ((void*)username);
			if (!pwd) {
				/* entry not found */
				/*errno=KDB_ERR_NOTFOUND;*/
				errno = errnosave;
				return -1;
			}
		} else {
			/* parent key not a valid directory */
			errno = errnosave;
			return -1;
		}
	}

	errno = errnosave;
	return nr_keys; /* success */
}


/**
 * Implementation for kdbSet() method.
 *
 * @see kdbSet() for caller.
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 * @return the number of keys set
 * @return 0 on success with no changed key in database
 * @return -1 on failure, the current key in returned shows the position and/or errno is set
 * @ingroup backend
 */
ssize_t kdbSet_passwd(KDB *handle, KeySet *returned, const Key *parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0;
	Key *key;
	PasswdData *data;
	const char *parent;
	int parent_len;

	parent=keyName(parentKey);
	parent_len=strlen(parent);

	data=kdbhGetBackendData(handle);

	if (parent_len<data->mountpointlen-1 || strncmp(data->mountpoint,parent,data->mountpointlen-1)) {
		// parentkey covers keys not handled by the backend
		errno = errnosave;
		return -1;
	}

	ksRewind(returned);
	key=ksNext(returned);
	for (;key;ksNext(returned)) {
		const char *name=keyName(key);
		int name_len=strlen(name);

		if (name_len>data->mountpointlen-1 && !strncmp(data->mountpoint,name,name_len)) {
			/* keyname is mounted name. This key is not writeable */

		} else if (name_len>data->mountpointlen && !strncmp(data->mountpoint,name,data->mountpointlen)) {

		} else {

		}
	}
	/* set all keys below parentKey and count them with nr_keys */
	errno = errnosave;
	return nr_keys;
}

/**
 * All KDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup backend
 */
KDB *KDBEXPORT(passwd)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_passwd,
		KDB_BE_CLOSE,	&kdbClose_passwd,
		KDB_BE_GET,	&kdbGet_passwd,
		KDB_BE_SET,	&kdbSet_passwd,
		KDB_BE_VERSION,	BACKENDVERSION,
		KDB_BE_AUTHOR,	"Patrick Sabin <patricksabin@gmx.at>",
		KDB_BE_LICENCE,	"BSD",
		KDB_BE_DESCRIPTION,
			"Reads and writes /etc/passwd content",
		KDB_BE_END);
}
