/***************************************************************************
            berkeleydb.c  -  A Berkeley DB backend for Elektra
                             -------------------
    begin                : Mon Jan 24 2005
    copyright            : (C) 2005 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/* Subversion stuff

$Id$

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <sys/types.h>
#include <pwd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <dirent.h>
#include <unistd.h>
#include <db.h>

#include <kdbbackend.h>

#define BACKENDNAME "berkeleydb"

#define DB_DIR_USER   ".kdb-berkeleydb"
#define DB_DIR_SYSTEM "/etc/kdb-berkeleydb"

#define DB_KEYVALUE      "keyvaluepairs"
#define DB_PARENTINDEX   "parentindex"

#define DB_FILE_KEYVALUE   "keyvalue.db"
#define DB_FILE_PARENTS    "parents.idx"

/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif



/**
 * Our DB layout uses 2 simple tables:
 * 
 * - keyValuePairs: table key is key name, and data is a serialization of
 *   key's metadata, value and comment.
 * 
 * - parentIndex: a secondary index, to make folder searches possible, so
 *   it contains the parent key name as the table key and some DB internal
 *   data to point to keyValuePairs table primary key-data pairs.
 * 
 * So if we have the following Elektra keys:
 * 
 *	user/sw/app1/key1
 *	user/sw/app1/key2
 *	user/sw/app1/dir1/
 *	user/sw/app1/dir1/key1
 *	user/sw/app1/dir1/key2
 * 
 * The keyValuePairs table will contain:
 * 
 *	user/sw/app1/key1      | metadata, value, comment
 *	user/sw/app1/key2      | metadata, value, comment
 *	user/sw/app1/dir1      | metadata, value, comment
 *	user/sw/app1/dir1/key1 | metadata, value, comment
 *	user/sw/app1/dir1/key2 | metadata, value, comment
 *	
 * And parentIndex table will contain:
 * 
 *	user/sw/app1      | (BDB internal pointer to key1 on primary table)
 *	user/sw/app1      | (BDB internal pointer to key2 on primary table)
 *	user/sw/app1      | (BDB internal pointer to dir1 on primary table)
 *	user/sw/app1/dir1 | (BDB internal pointer to dir1/key1 on primary table)
 *	user/sw/app1/dir1 | (BDB internal pointer to dir1/key2 on primary table)
 * 
 * The parentIndex table is written and managed automatically by Berkeley DB's
 * DB->associate() method, with the help of our parentIndexCallback().
 * 
 */




/**
 *  A container for the Berkeley DBs related to the same DBTree.
 */
typedef struct {
	DB *parentIndex;   /* maps folder names to the keys they contain */
	DB *keyValuePairs; /* maps keynames to their values + metainfo */
} DBInternals;





/**
 *  Each DBTree contains all info needed to access an Elektra
 *  root tree.
 *
 *  Example of root trees:
 *  system/ *        {isSystem=1,userDomain=0,...}
 *  user/ *          {isSystem=0,userDomain=$USER,...}
 *  user:luciana/ *  {isSystem=0,userDomain=luciana,...}
 *  user:denise/ *   {isSystem=0,userDomain=denise,...}
 *  user:tatiana/ *  {isSystem=0,userDomain=tatiana,...}
 *
 */
typedef struct _DBTree {
	/* if isSystem==0 and userDomain==0, this DB is for the current user */
	int isSystem;
	char *userDomain;
	DBInternals db;
	struct _DBTree *next;
} DBTree;






/**
 *  A container for all opened DBTrees
 */
typedef struct {
	size_t size;       /* number of opened databases */
	DBTree *cursor;
	DBTree *first;     /* databases */
} DBContainer;


/*
DBContainer *dbs=0;
*/



/**
 * Serialize a Key struct into DBT structs, one for key name
 * (including ending NULL), another for the rest.
 *
 * Memory will be allocated for DBT.data part, so it is caller
 * responsability to deallocate that latter.
 *
 */
int keyToBDB(const Key *key, DBT *dbkey, DBT *dbdata) {
	void *serialized;
	size_t metaInfoSize;
	int utf8Conversion=0, utf8CommentConverted=0, utf8ValueConverted = 0;
	char *convertedName=key->key;
	size_t sizeName=strblen(key->key);
	char *convertedValue=key->data;
	size_t sizeValue=key->dataSize;
	char *convertedComment=key->comment;
	size_t sizeComment=key->commentSize;


	/* First convert all to UTF-8 */
	if ((utf8Conversion=kdbNeedsUTF8Conversion())) {
		if (key->key) {
			convertedName=malloc(sizeName);
			memcpy(convertedName,key->key,sizeName);
			UTF8Engine(UTF8_TO,&convertedName,&sizeName);
		} else convertedName=key->key;

		if (dbdata) {
			if (!keyIsBin(key)) {
				convertedValue=malloc(sizeValue);
				memcpy(convertedValue,key->data,sizeValue);
				UTF8Engine(UTF8_TO,&convertedValue,&sizeValue);
				utf8ValueConverted = 1;
			} else convertedValue=key->data;
		 
			if (key->comment) {
				convertedComment=malloc(sizeComment);
				memcpy(convertedComment,key->comment,sizeComment);
				UTF8Engine(UTF8_TO,&convertedComment,&sizeComment);
				utf8CommentConverted = 1;
			} else convertedComment=key->comment;
		}
	} 
	
	if (dbdata) {
		memset(dbdata, 0, sizeof(DBT));

		metaInfoSize = KEY_METAINFO_SIZE(key);
		
		dbdata->size = metaInfoSize + sizeValue + sizeComment;
		serialized = malloc(dbdata->size);
		memset(serialized,0,dbdata->size);

		/* First part: the metainfo */
		memcpy(serialized,key,metaInfoSize);
		/* *((Key *)serialized)=*key; */

		/* Second part: the comment */
		memcpy(serialized+metaInfoSize,convertedComment,sizeComment);
		/* adjust comment size from UTF-8 conversion */
		if (key->commentSize!=sizeComment)
			memcpy(serialized+metaInfoSize-
				sizeof(key->commentSize)-sizeof(key->dataSize),
				&sizeComment,sizeof(sizeComment));
		
		/* Third part: the value */
		memcpy(serialized+metaInfoSize+sizeComment,convertedValue,sizeValue);
		/* adjust value size from UTF-8 conversion */
		if (key->dataSize!=sizeValue) 
			memcpy(serialized+metaInfoSize-sizeof(key->dataSize),
				&sizeValue,sizeof(sizeValue));
	
		dbdata->data=serialized;
		
		if (utf8CommentConverted )
			free(convertedComment);
		if ( utf8ValueConverted )
			free(convertedValue);
	}
	
	memset(dbkey, 0, sizeof(DBT));
	if (utf8Conversion) {
		dbkey->size=sizeName;
		dbkey->data=convertedName;
	} else {
		dbkey->size=strblen(key->key);
		dbkey->data=malloc(dbkey->size);
		strcpy(dbkey->data,key->key);
	}
	
	return 0;
}




/**
 * The oposite of keyToBDB.
 * Will take 2 DBTs (one for key name, other for data) and convert them
 * into a Key structure.
 * 
 * WARNING: key->userDomain must be set outside keyFromBDB(). Someplace more
 * aware of the context. So everywhere keyFromBDB is called, a call
 * to keySetOwner() should apper right after it.
 */
int keyFromBDB(Key *key, const DBT *dbkey, const DBT *dbdata) {
	size_t metaInfoSize;

	keyClose(key);
	
	metaInfoSize = KEY_METAINFO_SIZE(key);
	
	/* Set all metainfo */
	memcpy(key,        /* destination */
		dbdata->data,    /* source */
		metaInfoSize);   /* size */
	key->recordSize=dbdata->size;
	
	key->flags = KEY_SWITCH_INITIALIZED;

	/* Set comment */
	if (key->commentSize)
		keySetComment(key,dbdata->data+metaInfoSize);
	
	/* userDomain must be set outside this function,
	 * someplace more aware of the context */
	keySetName(key,dbkey->data);

	/* Set value. Key type came from the metaInfo importing above. */
	keySetRaw(key,dbdata->data+metaInfoSize+key->commentSize,key->dataSize);
	
	if (kdbNeedsUTF8Conversion()) {
		size_t size=strblen(key->key);
		
		UTF8Engine(UTF8_FROM,&key->key,&size);
		UTF8Engine(UTF8_FROM,&key->comment,&key->commentSize);
		if (!keyIsBin(key))
			UTF8Engine(UTF8_FROM,(char **)&key->data, &key->dataSize);
	}
	
	/* since we just got the key from the storage, it is synced. */
	key->flags &= ~KEY_SWITCH_NEEDSYNC;

	return 0;
}





/**
 * Calculates the secondary index for a key.
 * In our DB layout, the secondary index is simply the parent of the key.
 * This method is called everytime DB->get, DB->put etc BDB methods
 * are called.
 */
int parentIndexCallback(DB *db, const DBT *rkey, const DBT *rdata, DBT *pkey) {
	size_t baseNameSize,parentNameSize;
	char *parentPrivateCopy=0;

	baseNameSize=keyNameGetBaseNameSize(rkey->data);
	if (baseNameSize == 0)
		/* this is a root or empty key */
		return DB_DONOTINDEX;

	memset(pkey, 0, sizeof(DBT));

	parentNameSize=rkey->size-baseNameSize;
	parentPrivateCopy=malloc(parentNameSize);

	if (parentPrivateCopy) {
		memcpy(parentPrivateCopy,rkey->data,parentNameSize-1);
		parentPrivateCopy[parentNameSize-1]=0;
	}

	pkey->data=parentPrivateCopy;
	pkey->size=parentNameSize;
	pkey->flags=DB_DBT_APPMALLOC;

	return 0;
}







/**
 * Closes databases, frees internal memory and destroys the
 * DBTree data structure. It is the oposite of dbTreeNew().
 */
int dbTreeDel(DBTree *dbtree) {
	if (dbtree->userDomain) free(dbtree->userDomain);
	if (dbtree->db.keyValuePairs)
		dbtree->db.keyValuePairs->close(dbtree->db.keyValuePairs,0);
	if (dbtree->db.parentIndex)
		dbtree->db.parentIndex->close(dbtree->db.parentIndex,0);
	
	free(dbtree);
	
	return 0;
}




/**
 * Given a created, opened and empty DBTree, initialize its root key.
 * This is usually called by dbTreeNew().
 */
int dbTreeInit(KDBHandle handle,DBTree *newDB) {
	Key *root=0;
	int ret;
	mode_t mask;
	DBT dbkey,data;

	/* TODO: review security bits issues on daemon mode */
	if (newDB->isSystem) {
		root=keyNew("system",
			KEY_SWITCH_UID,0,
			KEY_SWITCH_GID,0,
			KEY_SWITCH_END);
	} else {
		struct passwd *userOwner;
		userOwner=getpwnam(newDB->userDomain);
		root=keyNew("user",
			KEY_SWITCH_UMODE, kdbhGetUMask(handle),
			KEY_SWITCH_UID,   kdbhGetUID(handle),
			KEY_SWITCH_GID,   kdbhGetGID(handle),
			KEY_SWITCH_TYPE,  KEY_TYPE_DIR,
			KEY_SWITCH_END);
	}

	mask=umask(0); umask(mask);
	keySetDir(root,mask);
	
	root->atime=root->mtime=root->ctime=time(0); /* set current time */

	keyToBDB(root,&dbkey,&data);

	ret = newDB->db.keyValuePairs->put(newDB->db.keyValuePairs,
		0,&dbkey,&data, 0);
	if (!ret) printf("db: %s: DB Initialized.\n", (char *)dbkey.data);
	else {
		newDB->db.keyValuePairs->err(newDB->db.keyValuePairs, ret, "DB->put");
		perror("DB->put");
	}

	keyDel(root);
	free(dbkey.data); dbkey.data=0;
	free(data.data); data.data=0;

	newDB->db.keyValuePairs->sync(newDB->db.keyValuePairs,0);

	return KDB_RET_OK;
}






/**
 * Tries to open a DB for a key.
 * If it doesn't exist, try to create it.
 * The returned new DBTree must be included in the static single
 * DBContainer by the caller.
 * The returned DBTree must be deleted later with dbTreeDel().
 * 
 * The DB location on the filesystem is something like this:
 * 
 * if (keyIsUser(forKey))
 *	~{keyGetOwner(forKey)}/.kdb-berkeleydb/{dbfiles}
 * else
 *	/etc/kdb-berkeleydb/{dbfiles}
 */
DBTree *dbTreeNew(KDBHandle handle,const Key *forKey) {
	DBTree *newDB;
	int ret;
	int newlyCreated; /* True if this is a new database */
	uid_t uid=0;
	gid_t gid=0;
	char dbDir[MAX_PATH_LENGTH];
	char parentsFile[MAX_PATH_LENGTH];
	char keyvalueFile[MAX_PATH_LENGTH];
	struct passwd *user=0;

	struct stat dbDirInfo;


	/***********
	 * Calculate path and filenames for the DB files.
	 ***********/

	if (keyIsSystem(forKey)) {
		/* Prepare to open the 'system/ *' database */
		strcpy(dbDir,DB_DIR_SYSTEM);
		uid = 0;
		gid = 0;
	} else if (keyIsUser(forKey)) {
		/* Prepare to open the 'user:????.*' database */
		/* TODO: user should be calculated from handle */
		user=getpwnam(forKey->userDomain);
		sprintf(dbDir,"%s/%s",user->pw_dir,DB_DIR_USER);
		uid = user->pw_uid;
		gid = user->pw_gid;
	}

	if (stat(dbDir,&dbDirInfo)) {
		/* Directory does not exist. create it */
		int ret;
		
		fprintf(stderr,"Going to create dir %s\n",dbDir);
		ret=mkdir(dbDir,DEFFILEMODE | S_IXUSR);
		if (ret) return 0; /* propagate errno */
		chown(dbDir,  uid, gid);
	} else {
		/* Something exist there. Check it first */
		if (!S_ISDIR(dbDirInfo.st_mode)) {
			/* It is not a directory ! */
			errno=EACCES;
			return 0;
		}
	}

	sprintf(keyvalueFile,"%s/%s",dbDir,DB_FILE_KEYVALUE);
	sprintf(parentsFile,"%s/%s",dbDir,DB_FILE_PARENTS);

	newDB=malloc(sizeof(DBTree));
	memset(newDB,0,sizeof(DBTree));
	newDB->isSystem=keyIsSystem(forKey);
	newlyCreated=0;


	/* We have the files names. Now open/create them */

	/****************
	 * The main database. The one you can find the real key-value pairs
	 *****************/
	if ((ret = db_create(&newDB->db.keyValuePairs, NULL, 0)) != 0) {
		fprintf(stderr, "db_create: %s: %s\n", DB_KEYVALUE, db_strerror(ret));
		free(newDB);
		errno=KDB_RET_EBACKEND;
		return 0;
	}
	ret=newDB->db.keyValuePairs->open(newDB->db.keyValuePairs,NULL,keyvalueFile,
		DB_KEYVALUE, DB_BTREE, DB_CREATE | DB_EXCL | DB_THREAD, 0);
	if (ret == EEXIST || ret == EACCES) {
		/* DB already exist. Only open it */
		ret=newDB->db.keyValuePairs->open(newDB->db.keyValuePairs,NULL, keyvalueFile,
			DB_KEYVALUE, DB_BTREE, DB_THREAD, 0);
		if (ret == EACCES)
			ret=newDB->db.keyValuePairs->open(newDB->db.keyValuePairs,NULL,
				keyvalueFile, DB_KEYVALUE, DB_BTREE, DB_THREAD | DB_RDONLY, 0);
	} else newlyCreated=1;

	
	if (ret) {
		newDB->db.keyValuePairs->err(newDB->db.keyValuePairs,
			ret, "%s", DB_KEYVALUE);
		dbTreeDel(newDB);
		errno=KDB_RET_EBACKEND;
		return 0;
	}




	/* TODO: Check newlyCreated also */
	/****************
	 * The parent index. To make key searches by their parents
	 *****************/
	ret=db_create(&newDB->db.parentIndex, NULL, 0);
	if (ret != 0) {
		fprintf(stderr, "db_create: %s: %s\n", DB_PARENTINDEX, db_strerror(ret));
		dbTreeDel(newDB);
		errno=KDB_RET_EBACKEND;
		return 0;
	}
	
	ret = newDB->db.parentIndex->set_flags(newDB->db.parentIndex,
		DB_DUP | DB_DUPSORT);
	if (ret != 0) fprintf(stderr, "set_flags: %s: %d\n",DB_PARENTINDEX,ret);
	
	ret = newDB->db.parentIndex->open(newDB->db.parentIndex,
		NULL, parentsFile, DB_PARENTINDEX, DB_BTREE, DB_CREATE | DB_EXCL | DB_THREAD, 0);
	if (ret == EEXIST || ret == EACCES) {
		/* DB already exist. Only open it */
		ret=newDB->db.parentIndex->open(newDB->db.parentIndex,NULL, parentsFile,
			DB_PARENTINDEX, DB_BTREE, DB_THREAD, 0);
		if (ret == EACCES)
			ret=newDB->db.parentIndex->open(newDB->db.parentIndex,NULL,
				parentsFile, DB_PARENTINDEX, DB_BTREE, DB_THREAD | DB_RDONLY, 0);
	}
	
	if (ret) {
		newDB->db.parentIndex->err(newDB->db.parentIndex, ret, "%s", DB_PARENTINDEX);
		dbTreeDel(newDB); 
		errno=KDB_RET_EBACKEND;
		return 0;
	}
	
	ret = newDB->db.keyValuePairs->associate(newDB->db.keyValuePairs, NULL,
		newDB->db.parentIndex, parentIndexCallback, DB_DBT_APPMALLOC);
	if (ret != 0) {
		fprintf(stderr, "error: %s: %d\n",DB_PARENTINDEX,ret);
		dbTreeDel(newDB);
		errno=KDB_RET_EBACKEND;
		return 0;
	}




	if (!newDB->isSystem) {
		newDB->userDomain=malloc(strblen(forKey->userDomain));
		strcpy(newDB->userDomain,forKey->userDomain);
	}

	/* Set file permissions for the DB files */
	if (newlyCreated) {
		if (user) {
			chown(keyvalueFile,  user->pw_uid,user->pw_gid);
			chown(parentsFile,  user->pw_uid,user->pw_gid);
		}
		dbTreeInit(handle,newDB); /* populate */
	}
	return newDB;
}









/**
 * Return the DB suitable for the key.
 * Lookup in the list of opened DBs (DBContainer). If not found, tries to
 * open it with dbTreeNew().
 * Key name and user domain will be used to find the correct database.
 */
DBTree *getDBForKey(KDBHandle handle, const Key *key) {
	DBContainer *dbs=kdbhGetBackendData(handle);
	DBTree *current,*newDB;
	char rootName[100];
	rootName[0]=0; /* just to be sure... */

	if (dbs->cursor) current=dbs->cursor;
	else current=dbs->cursor=dbs->first;
	
	/* We found some DB opened.
	 * Browse it starting from the cursor. */
	if (current) {
		/* Look for a DB in our opened DBs */
		if (keyIsSystem(key))
			do {
				if (current->isSystem) return dbs->cursor=current;
			
				current=current->next;
				if (!current) current=dbs->first;
			} while (current && current!=dbs->cursor);
		else if (keyIsUser(key)) {
			/* If key is a user key, it can't have an empty userDomain */
			if (key->userDomain == 0) return 0;
			do {
				if (!current->isSystem && !strcmp(key->userDomain,current->userDomain))
					return dbs->cursor=current;
				
				current=current->next;
				if (!current) current=dbs->first;
			} while (current && current!=dbs->cursor);
		}
	}
	
	/* If we reached this point, the DB for our key is not in our container.
	 * Open it and include in the container. */

	newDB=dbTreeNew(handle,key);
	if (newDB) {
		/* Put the new DB right after the container's current DB (cursor).
		 * And set the cursor to be the new DB. */
		if (dbs->cursor) {
			newDB->next=dbs->cursor->next;
			dbs->cursor->next=newDB;
			dbs->cursor=newDB;
		} else dbs->cursor=dbs->first=newDB;
		dbs->size++;
	}
	
	/* If some error ocurred inside dbTreeNew(), errno will be propagated */
	
	return dbs->cursor;
}







/*************************************************
 * Interface Implementation
 *************************************************/





int kdbOpen_bdb(KDBHandle *handle) {
	/* Create only the DB container.
	 * DBs will be allocated on demand
	 */
	DBContainer *dbs;
	
	dbs=malloc(sizeof(DBContainer));
	memset(dbs,0,sizeof(DBContainer));
	
	kdbhSetBackendData(*handle,dbs);
	
	return 0;
}




int kdbClose_bdb(KDBHandle *handle) {
	DBContainer *dbs;
	
	dbs=kdbhGetBackendData(*handle);
	
	if (dbs) {
		while (dbs->first) {
			dbs->cursor=dbs->first;
			dbs->first=dbs->cursor->next;

			dbTreeDel(dbs->cursor);
		}
		free(dbs); dbs=0;
	}
	return 0; /* success */
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemoveKey() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_bdb(KDBHandle handle, const Key *key) {
	DBContainer *dbs;
	DBTree *dbctx;
	DBT dbkey,data;
	int ret;
	uid_t user=kdbhGetUID(handle);
	gid_t group=kdbhGetGID(handle);
	int canWrite=0;
	int hasChild=0;
	Key *cast=0;
	
	dbs=kdbhGetBackendData(handle);
	
	dbctx=getDBForKey(handle,key);
	if (!dbctx) return 1; /* propagate errno from getDBForKey() */
	
	/* First check if we have write permission to the key */
	memset(&dbkey,0,sizeof(DBT));
	memset(&data,0,sizeof(DBT));
	dbkey.size=dbkey.ulen=strblen(key->key);
	dbkey.data=key->key;
	data.flags=DB_DBT_REALLOC;
	

	/* Check if key exists on the database */
	ret = dbctx->db.keyValuePairs->get(dbctx->db.keyValuePairs,
		NULL, &dbkey, &data, 0);
		
	if (ret == DB_NOTFOUND) return errno=KDB_RET_NOTFOUND;
	
	if (ret == 0) {
		/* DB entry for key found */
		cast=(Key *)data.data;

		/* Check parent permissions to write bellow it. */
		if (cast->uid == user)
			canWrite = cast->access & S_IWUSR;
		else if (cast->gid == group)
			canWrite = cast->access & S_IWGRP;
		else canWrite= cast->access & S_IWOTH;
	}
	


	if ( canWrite && (ret == 0)) {
		/* Check if key is a dir and have children */
		
		if (keyIsDir(cast) /* safe, because it looks only on metadata */) {
			if (data.data) free(data.data), data.data=0;
			ret = dbctx->db.parentIndex->get(dbctx->db.parentIndex,
				NULL, &dbkey, &data, 0);

			if (ret == 0) hasChild=1;
			else if (ret == DB_NOTFOUND) hasChild=0;
		}
	}


	if (data.data) free(data.data),data.data=0;
	
	if (! canWrite) return errno=KDB_RET_NOCRED;
	if (hasChild) return errno=ENOTEMPTY;


	/* Ok, so we can delete the key */
	ret=dbctx->db.keyValuePairs->del(dbctx->db.keyValuePairs,
		NULL, &dbkey, 0);
	
	switch (ret) {
		case 0:
			return ret; /* success */
			break;
		case EACCES:
			return errno=KDB_RET_NOCRED;
			break;
		default:
			dbctx->db.keyValuePairs->err(dbctx->db.keyValuePairs, ret, "DB->del");
	}
	
	return ret;
}


int kdbGetKeyWithOptions(KDBHandle handle, Key *key, uint32_t options) {
	DBContainer *dbs;
	DBTree *dbctx;
	DBT dbkey,data;
	int ret;
	uid_t user=kdbhGetUID(handle);
	gid_t group=kdbhGetGID(handle);
	int canRead=0;
	int isLink=0;
	Key buffer;

	dbs=kdbhGetBackendData(handle);
	
	dbctx=getDBForKey(handle,key);
	if (!dbctx) return 1; /* propagate errno from getDBForKey() */

	keyInit(&buffer);
	memset(&dbkey,0,sizeof(DBT));
	memset(&data,0,sizeof(DBT));
	dbkey.size=dbkey.ulen=strblen(key->key);
	dbkey.data=key->key;
	data.flags=DB_DBT_REALLOC;

	ret = dbctx->db.keyValuePairs->get(dbctx->db.keyValuePairs,
		NULL, &dbkey, &data, 0);
		
	switch (ret) {
		case 0: { /* Key found and retrieved. Check permissions */
			keyFromBDB(&buffer,&dbkey,&data);
			if (keyIsUser(&buffer)) keySetOwner(&buffer,dbctx->userDomain);

			/* Keep key position in its keyset */
			buffer.next = key->next;
			
			dbkey.data=0;
			free(data.data); data.data=0;
			
			/* End of BDB specific code in this method */
			
			
			/* Check permissions. */
			if (keyGetUID(&buffer) == user)
				canRead = keyGetAccess(&buffer) & S_IRUSR;
			else if (keyGetGID(&buffer) == group)
				canRead = keyGetAccess(&buffer) & S_IRGRP;
			else canRead = keyGetAccess(&buffer) & S_IROTH;

			if (!canRead) {
				keyClose(&buffer);
				return errno=KDB_RET_NOCRED;
			}
			break;
		}
		case DB_NOTFOUND:
			return errno=KDB_RET_NOTFOUND;
			break;
	}

	isLink=keyIsLink(&buffer);
	
	if (canRead) {
		if (!isLink && (options & KDB_O_STATONLY))
			keySetRaw(&buffer,0,0);
		if (isLink && !(options & KDB_O_NFOLLOWLINK)) {
			/* If we have a link and user did not specify KDB_O_NFOLLOWLINK,
			 * he want to dereference the link */
			Key target;
			
			keyInit(&target);
			keySetName(&target,buffer.data);

			if (kdbGetKeyWithOptions(handle,&target, options) ==
					KDB_RET_NOTFOUND) {
				keyClose(&target);
				keyClose(&buffer);
				return errno=KDB_RET_NOTFOUND;
			}
		}
	}
	keyDup(&buffer,key);
	keyClose(&buffer);
	
	return KDB_RET_OK; /* success */
}





int kdbGetKey_bdb(KDBHandle handle, Key *key) {
	return kdbGetKeyWithOptions(handle,key,0);
}



int kdbStatKey_bdb(KDBHandle handle, Key *key) {
	return kdbGetKeyWithOptions(handle,key,KDB_O_NFOLLOWLINK | KDB_O_STATONLY);
}



/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_bdb(KDBHandle handle, Key *key) {
	DBTree *dbctx;
	DBT dbkey,data;
	int ret;
	uid_t user=kdbhGetUID(handle);
	gid_t group=kdbhGetGID(handle);
	int canWrite=0;

	dbctx=getDBForKey(handle,key);
	if (!dbctx) return 1; /* propagate errno from getDBForKey() */

	/* Check access permissions.
	   Check if this client can commit this key to the database */

	memset(&dbkey,0,sizeof(DBT));
	memset(&data,0,sizeof(DBT));
	dbkey.size=dbkey.ulen=strblen(key->key);
	dbkey.data=key->key;
	dbkey.flags=data.flags=DB_DBT_REALLOC;

	ret = dbctx->db.keyValuePairs->get(dbctx->db.keyValuePairs,
		NULL, &dbkey, &data, 0);
		
	switch (ret) {
		case 0: { /* Key found and retrieved. Check permissions */
			Key *cast;
			
			cast=(Key *)data.data;
			
			/* Check parent permissions to write bellow it. */
			if (cast->uid == user)
				canWrite = cast->access & S_IWUSR;
			else if (cast->gid == group)
				canWrite = cast->access & S_IWGRP;
			else canWrite= cast->access & S_IWOTH;
			
			/* cleanup */
			dbkey.data=0;
			free(data.data); data.data=0;

			/* keyClose(&buffer); */
			break;
		}
		case DB_NOTFOUND: {
			/* We don't have this key yet.
			   Check if we have a parent and its permissions. */
			Key *parent=0;
			size_t parentNameSize;
			char *parentName;

			parentNameSize=keyGetParentNameSize(key);
			parentName=malloc(parentNameSize);
			keyGetParentName(key,parentName,parentNameSize);
			
			memset(&dbkey,0,sizeof(DBT));
			memset(&data,0,sizeof(DBT));
			dbkey.data=parentName;
			dbkey.size=parentNameSize;
			dbkey.flags=data.flags=DB_DBT_REALLOC;

			ret = dbctx->db.keyValuePairs->get(dbctx->db.keyValuePairs, NULL,
				&dbkey, &data, 0);

			if (ret == DB_NOTFOUND) {
				/* No, we don't have a parent. Create dirs recursivelly */
				
				parent=keyNew(KEY_SWITCH_END);
				
				/* explicitly set these attributes from the handle cause we
				 * could be running under a daemon context */
				keySetUID(parent,user);
				keySetGID(parent,group);
				keySetDir(parent,kdbhGetUMask(handle));
				
				/* Next block exist just to not call 
				 * keySetName(), a very expensive method.
				 * This is a not-recomended hack. */
				parent->key=parentName;
				parent->flags |= key->flags & 
					(KEY_SWITCH_ISSYSTEM | KEY_SWITCH_ISUSER);
				parent->userDomain=key->userDomain;
				
				/* free(parentName); */
				
				if (kdbSetKey_bdb(handle,parent)) {
					/* If some error happened in this recursive call.
					 * Propagate errno.
					 */
					
					/* disassociate our hack for deletion */
					parent->userDomain=0;
					
					/* parentName will be free()d here too */
					keyDel(parent);
					
					return 1;
				}
				
				/* disassociate our hack for latter deletion */
				parent->userDomain=0;
				
				/* data.data enters and quits this block empty */
			} else {
				/* Yes, we have a parent already. */
				/*parent=keyNew(0);
				keyFromBDB(parent,&dbkey,&data);
				keySetOwner(parent,dbctx->userDomain);
				
				free(data.data);
				*/
				
				/* we don't need it anymore */
				free(parentName);
				
				/* we are only interested in some metainfo, so just cast it */
				parent=(Key *)data.data;
			}

			/* Check parent permissions to write bellow it. */
			if (parent->uid == user)
				canWrite = parent->access & S_IWUSR;
			else if (parent->gid == group)
				canWrite = parent->access & S_IWGRP;
			else canWrite= parent->access & S_IWOTH;
			
			if (data.data) free(data.data);
			
			if (parent == (Key *)data.data) parent = 0;
			else if (parent) keyDel(parent);
			
			break;
		} /* case DB_NOTFOUND */
	} /* switch */

	if (! canWrite) return errno=KDB_RET_NOCRED;

	key->mtime=key->atime=time(0); /* set current time into key */
	keyToBDB(key,&dbkey,&data);

	if ((ret = dbctx->db.keyValuePairs->put(dbctx->db.keyValuePairs,
			NULL, &dbkey, &data, 0)) != 0) {
		dbctx->db.keyValuePairs->err(dbctx->db.keyValuePairs, ret, "DB->put");
		
		free(dbkey.data); dbkey.data=0;
		free(data.data); data.data=0;

		errno=KDB_RET_NOCRED; /* probably this is the error */
		return 1;
	}

	free(dbkey.data); dbkey.data=0;
	free(data.data); data.data=0;

	/* Mark the key as synced */
	key->flags &= ~KEY_SWITCH_NEEDSYNC;

	/*
	dbctx->db.keyValuePairs->sync(dbctx->db.keyValuePairs,0);
	dbctx->db.parentIndex->sync(dbctx->db.parentIndex,0);
	*/
	return 0; /* success */
}



/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
ssize_t kdbGetKeyChildKeys_bdb(KDBHandle handle, const Key *parentKey,
		KeySet *returned, unsigned long options) {
	DBTree *db=0;
	DBC *cursor=0;
	DBT parent,keyName,keyData;
	Key *currentParent, *retrievedKey;
	KeySet folders;
	uid_t user=kdbhGetUID(handle);
	gid_t group=kdbhGetGID(handle);
	mode_t canRead=0; /* wether we have permissions to go ahead */
	int ret=0;
	
	/* Get/create the DB for the parent key */
	db=getDBForKey(handle,parentKey);

	if (db == 0) {
		/* Bizarre sitution when a DB (existing or newly creted) can't be
		   associted with the passed key. This is unacceptable and trated as
		   as INVALID, because all DBs are calculated from key name.
		*/
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}

	currentParent=keyNew(KEY_SWITCH_END);
	keyDup(parentKey,currentParent);
	ret=kdbGetKeyWithOptions(handle,currentParent,KDB_O_STATONLY);

	if (ret==KDB_RET_NOTFOUND) {
		keyDel(currentParent);
		errno=KDB_RET_NOTFOUND;
		return -1;
	}
	
	if (! keyIsDir(currentParent)) {
		/* TODO: dereference link keys */
		keyDel(currentParent);
		errno=ENOTDIR;
		return -1;
	}

	/* Check master parent permissions from DB */
	if (currentParent->uid == user)
		canRead = currentParent->access & (S_IRUSR | S_IXUSR);
	else if (currentParent->gid == group)
		canRead = currentParent->access & (S_IRGRP | S_IXGRP);
	else canRead = currentParent->access & (S_IROTH | S_IXOTH);
	
	keyDel(currentParent);
	
	if (!canRead) return errno=KDB_RET_NOCRED;

	/* initialize the KeySet that will hold the fetched folders */
	ksInit(&folders);
	
	/* initialize a cursor to walk through each key under a folder */
	ret = db->db.parentIndex->cursor(db->db.parentIndex, NULL, &cursor, 0);
	
	currentParent=(Key *)parentKey;
	
	/*
	 * Each loop pass reads all keys from a single folder, without recursion.
	 * Recursion is provided by multiple passes on this loop
	 */
	do {
		memset(&parent,0,sizeof(parent));
		memset(&keyData,0,sizeof(keyData));
		memset(&keyName,0,sizeof(keyName));
		
		/* Memory will be allocated now for the DBTs.... */
		keyToBDB((const Key *)currentParent,&parent,0);
	
		/* Let BDB allocate memory for next key name and data */
		keyName.flags=keyData.flags=DB_DBT_REALLOC;
		
		/* position the cursor in the first key of "parent" folder
		   and retrieve it */
		ret=cursor->c_pget(cursor,
				&parent,
				&keyName,
				&keyData,
				DB_SET);
		
		if (ret == DB_NOTFOUND) {
			/* We are probably in a root key, that
			 * doesn't have any subentry in the index. */
			free(parent.data);
			free(keyName.data);
			free(keyData.data);
			break;
			/*return KDB_RET_NOTFOUND; */
		}
		
		/* Now start retrieving all child keys */
		do { /* next cursor move is in the ending "while" */
		
			/* Check if is inactive before doing higher level operations */
			if (!(options & KDB_O_INACTIVE)) {
				char *sep;
				
				/* If we don't want inactive keys, check if its inactive */
				/* TODO: handle escaping */
				sep=strrchr((char *)keyName.data,RG_KEY_DELIM);
				if (sep && sep[1] == '.') {
					/* This is an inactive key, and we don't want it */
					/* Ignore this key, free all, and continue */
					
					free(keyName.data); free(keyData.data);
					memset(&keyName,0,sizeof(keyName));
					memset(&keyData,0,sizeof(keyData));
					keyName.flags=keyData.flags=DB_DBT_REALLOC;
		
					/* fetch next */
					continue;
				}
			}
		
			retrievedKey=keyNew(KEY_SWITCH_END);
			keyFromBDB(retrievedKey,&keyName,&keyData);
			if (keyIsUser(retrievedKey)) keySetOwner(retrievedKey,db->userDomain);
		
			free(keyName.data); free(keyData.data);
			memset(&keyName,0,sizeof(keyName));
			memset(&keyData,0,sizeof(keyData));
			keyName.flags=keyData.flags=DB_DBT_REALLOC;
		
			/* End of BDB specific code, ready for next c_pget() */
		
			/* check permissions for this key */
			canRead=0;
			if (options & KDB_O_STATONLY) {
				if (!keyIsLink(retrievedKey)) keySetRaw(retrievedKey,0,0);
				canRead=1;
			} else {
				/* If caller wants the value, comment, etc... */
				canRead=0;
				if (retrievedKey->uid == user) {
					canRead = (retrievedKey->access & S_IRUSR);
				} else if (retrievedKey->gid == group) {
					canRead = (retrievedKey->access & S_IRGRP);
				} else canRead = (retrievedKey->access & S_IROTH);
			}
		
			if (!canRead) {
				keyDel(retrievedKey);
				continue;
			}
		
			
			if (keyIsLink(retrievedKey) && !(options & KDB_O_NFOLLOWLINK)) {
			/* If we have a link and user did not specify KDB_O_NFOLLOWLINK,
			 * means he wants to dereference the link */
				Key target;
			
				keyInit(&target);
				keySetName(&target,retrievedKey->data);

				if (kdbGetKeyWithOptions(handle,&target, options) ==
						KDB_RET_NOTFOUND) {
					/* Invalid link target, so don't include in keyset */
				
					keyClose(&target);
				
					errno=KDB_RET_NOTFOUND;
					/* fetch next */
					continue;
				} else {
					keyDup(&target,retrievedKey);
					keyClose(&target);
				}
			}
		
			if (keyIsDir(retrievedKey)) {
				if (options & KDB_O_RECURSIVE) {
					ksAppend(&folders,retrievedKey);
				} else if (options & KDB_O_DIR) {
					ksAppend(returned,retrievedKey);
				} else keyDel(retrievedKey); /* discard */
			} else if (options & KDB_O_DIRONLY) {
				/* If key isn't a dir, and user only wants dirs... */
				keyDel(retrievedKey);
				retrievedKey=0;
			} else ksAppend(returned, retrievedKey);
		} while (0==(ret=cursor->c_pget(cursor,&parent,&keyName,&keyData,DB_NEXT_DUP)));
	} while ((currentParent=ksNext(&folders)));
	
	/* At this point we have all keys we want. Make final adjustments. */
	
	if (options & KDB_O_DIR)
		ksInsertKeys(returned,&folders);
	
	ksClose(&folders);
	
	if ((options & (KDB_O_SORT)) && (returned->size > 1))
		ksSort(returned);
	
	cursor->c_close(cursor);
	
	return returned->size;
}





/**
 * All KeyDB methods implemented by the backend can have random names, except
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
KDBEXPORT(berkeleydb) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_bdb,
		KDB_BE_CLOSE,          &kdbClose_bdb,
		KDB_BE_GETKEY,         &kdbGetKey_bdb,
		KDB_BE_SETKEY,         &kdbSetKey_bdb,
		KDB_BE_STATKEY,        &kdbStatKey_bdb,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_bdb,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_bdb,
		/* set to default implementation: 
		 * Again, don't set explicitly. See filesys for more info*/
/*		KDB_BE_RENAME,         &kdbRename_backend,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_default,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_default,
		KDB_BE_SETKEYS,        &kdbSetKeys_default,*/
		KDB_BE_END);
}
