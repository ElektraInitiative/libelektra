/***************************************************************************
                          key.c  -  Methods for key manipulation
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
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


#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h> 
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <langinfo.h>

#include "registry.h"
#include "registryprivate.h"

extern int errno;



size_t strblen(char *s) {
	char *found=index(s,0);
	if (found) return found-s+1;
	return 0;
}



int keyInit(Key *key) {
	if (!key) return errno=RG_KEY_RET_NULLKEY;

	memset(key,0,sizeof(Key));
	key->type=RG_KEY_TYPE_UNDEFINED;
	key->uid=getuid();
	key->gid=getgid();
	key->access=umask(0); umask(key->access);
	key->access=0666 & ~key->access;

	key->flags |= RG_KEY_FLAG_INITIALIZED | RG_KEY_FLAG_ACTIVE;

	return 0;
}




int keyClose(Key *key) {
	if (!key) return errno=RG_KEY_RET_NULLKEY;
	if (!keyIsInitialized(key)) return 0;

	free(key->key);
	free(key->data);
	free(key->comment);
	free(key->userDomain);
	memset(key,0,sizeof(Key));
	return 0;
}








int keyIsInitialized(Key *key) {
	if (!key) return 0;
	return ((key->flags & RG_KEY_FLAG_INITMASK)==RG_KEY_FLAG_INITIALIZED);
}




int keyNeedsSync(Key *key) {
	if (!key) return 0;
	return (key->flags & RG_KEY_FLAG_NEEDSYNC);
}



u_int8_t keyGetType(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return RG_KEY_TYPE_UNDEFINED;
	}

	return key->type;
}




u_int8_t keySetType(Key *key,u_int8_t newType) {
	mode_t dirSwitch=0111;

	if (!key) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return RG_KEY_TYPE_UNDEFINED;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	switch (newType) {
		case RG_KEY_TYPE_DIR:
			key->type=RG_KEY_TYPE_DIR;
			dirSwitch=umask(0); umask(dirSwitch);
			dirSwitch=0111 & ~dirSwitch;
			key->access|=dirSwitch | S_IFDIR;
			keySetRaw(key,0,0); /* remove data */
			break;
		default:
			key->type=newType;
			key->access &= ~(S_IFDIR | dirSwitch);
			key->flags |= RG_KEY_FLAG_NEEDSYNC;
	}
	return key->type;
}



size_t keyGetDataSize(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	return key->dataSize;
}


size_t keyGetRecordSize(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	return key->recordSize;
}




size_t keyGetNameSize(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (key->key) return strblen(key->key);
	else return 0;
}




size_t keyGetName(Key *key, char *returnedName, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->key) {
		errno=RG_KEY_RET_NOKEY;
		return 0;
	}

	bytes=strblen(strncpy(returnedName,key->key,maxSize));
	if (maxSize < strblen(key->key)) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	}
	return bytes;
}



size_t keyGetFullNameSize(Key *key) {
	size_t returnedSize;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	if (!key->key) return 0;

	returnedSize=strblen(key->key);

	if (!strncmp("user",key->key,sizeof("user")-1) && key->userDomain)
		returnedSize+=strblen(key->userDomain);

	return returnedSize;
}



size_t keyGetFullName(Key *key, char *returnedName, size_t maxSize) {
	size_t userSize=sizeof("user")-1;
	size_t userDomainSize,length;
	char *cursor;

	length=keyGetFullNameSize(key);
	if (length < 0) return length;
	if (length > maxSize) {
		errno=RG_KEY_RET_TRUNC;
		return -1;
	}

	cursor=returnedName;
	if (!strncmp("user",key->key,userSize)) {
		strncpy(cursor,key->key,userSize);
		cursor+=userSize;
		if (key->userDomain) {
			*cursor=':'; ++cursor;
			userDomainSize=strblen(key->userDomain)-1;
			strcpy(cursor,key->userDomain);
			cursor+=userDomainSize;
		}
		strcpy(cursor,key->key+userSize);
	} else strcpy(cursor,key->key);

	return length;
}






size_t keySetName(Key *key, char *newName) {
	size_t length;
	size_t rootLength, userLength, systemLength, userDomainLength;
	size_t keyNameSize=1; /* equal to length plus a space for \0 */

	if (!key) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	/* handle null new key name, removing the old key */
	if (!newName || !(length=strlen(newName))) {
		if (key->key) {
			free(key->key);
			key->key=0;
		}
		key->flags &= ~(RG_KEY_FLAG_HASKEY | RG_KEY_FLAG_NEEDSYNC);
		return 0;
	}

	/* Remove leading '/' if caller passed some */
	while (newName[length]==RG_KEY_DELIM) {
		length--;
	}

	rootLength=keyNameGetRootNameSize(newName);
	if (!rootLength) {
		errno=RG_KEY_RET_INVALIDKEY;
		return 0;
	}
	userLength=sizeof("user")-1;
	systemLength=sizeof("system")-1;
	userDomainLength=rootLength-userLength-1;
	if (userDomainLength<0) userDomainLength=0;

	if (!strncmp("user",newName,userLength<length?userLength:length)) {
		/* handle "user*" */
		if (length > userLength) {
			/* handle "user?*" */
			if (*(newName+userLength)==':') {
				/* handle "user:*" */
				if (userDomainLength > 0) {
					key->userDomain=realloc(key->userDomain,userDomainLength+1);
					strncpy(key->userDomain,newName+userLength+1,userDomainLength);
					key->userDomain[userDomainLength]=0;
				}
				keyNameSize+=length-userDomainLength-1;  /* -1 is for the ':' */
			} else if (*(newName+userLength)!=RG_KEY_DELIM) {
				/* handle when != "user/ *" */
				errno=RG_KEY_RET_INVALIDKEY;
				return 0;
			} else {
				/* handle regular "user/ *" */
				keyNameSize+=length;
			}
		} else {
			/* handle "user" */
			keyNameSize+=userLength;
		}
		
		key->key=realloc(key->key,keyNameSize);

		/* here key->key must have a correct size allocated buffer */
		if (!key->key) return 0;

		strcpy(key->key,"user");
		strncpy(key->key+userLength,newName+rootLength,length-rootLength);
		key->key[keyNameSize-1]=0;
		
		if (!key->userDomain) {
			size_t bsize=strblen(getenv("USER"));
			
			if (!bsize) {}
			else {
				key->userDomain=malloc(bsize);
				strncpy(key->userDomain,getenv("USER"),bsize);
			}
		}
		
	} else if (!strncmp("system",newName,systemLength<length?systemLength:length)) {
		/* handle "system*" */
		if (length > systemLength && *(newName+systemLength)!=RG_KEY_DELIM) {
			/* handle when != "system/ *" */
			errno=RG_KEY_RET_INVALIDKEY;
			return 0;
		}
		keyNameSize+=length;
		key->key=realloc(key->key,keyNameSize);

		/* here key->key must have a correct size allocated buffer */
		if (!key->key) return 0;

		strncpy(key->key,newName,length);
		key->key[keyNameSize-1]=0;
	} else {
		/* Passed name is neither "system" or "user" */
		errno=RG_KEY_RET_INVALIDKEY;
		return 0;
	}

	key->flags |= RG_KEY_FLAG_HASKEY | RG_KEY_FLAG_NEEDSYNC;

	return keyNameSize;
}



/** Extracts the user name from Key::key.
	Given 'user:someuser.*' return 'someuser'
	Given 'user:some\.user.*' return 'some.user'
*/
size_t keyGetOwner(Key *key, char *returned, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->userDomain) {
		errno=RG_KEY_RET_NODOMAIN;
		return 0;
	}

	if (maxSize < (bytes=strblen(key->userDomain))) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	} else strcpy(returned,key->userDomain);
	return bytes;
}



size_t keySetOwner(Key *key, char *userDomain) {
	size_t size;

	if (!key) {
		errno=RG_KEY_RET_UNINITIALIZED; /* RG_KEY_RET_NULLKEY */
		return -1;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	if ((size=strblen(userDomain)) > 0) {
		if (key->userDomain) {
			key->userDomain=realloc(key->userDomain,size);
		} else {
			key->userDomain=malloc(size);
		}
		if (!key->userDomain) return -1; /* propagate errno */

		strcpy(key->userDomain,userDomain);
		key->flags |= RG_KEY_FLAG_HASDOMAIN | RG_KEY_FLAG_NEEDSYNC;
		return size;
	} else if (key->userDomain) {
		free(key->userDomain);
		key->userDomain=0;
		key->flags &= ~(RG_KEY_FLAG_HASDOMAIN | RG_KEY_FLAG_NEEDSYNC);
	}
	return 0;
}





size_t keyGetComment(Key *key, char *returnedDesc, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->comment) {
		errno=RG_KEY_RET_NODESC;
		return 0;
	}

	bytes=strblen(strncpy(returnedDesc,key->comment,maxSize));
	if (maxSize < strblen(key->comment)) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	}
	return bytes;
}



size_t keyGetCommentSize(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->comment) {
		errno=RG_KEY_RET_NODESC;
		return 0;
	}

	return strblen(key->comment);
}





size_t keySetComment(Key *key, char *newComment) {
	size_t size;

	if (!key) {
		errno=RG_KEY_RET_UNINITIALIZED; /* RG_KEY_RET_NULLKEY */
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	if (newComment && (size=strblen(newComment)) > 0) {
		if (key->flags & RG_KEY_FLAG_HASCOMMENT) {
			key->comment=realloc(key->comment,size);
		} else {
			key->comment=malloc(size);
		}
		if (!key->comment) return 0;

		strcpy(key->comment,newComment);
		key->flags |= RG_KEY_FLAG_HASCOMMENT | RG_KEY_FLAG_NEEDSYNC;
		return key->commentSize=size;
	} else if (key->flags & RG_KEY_FLAG_HASCOMMENT) {
		free(key->comment);
		key->comment=0;
		key->flags &= ~(RG_KEY_FLAG_HASCOMMENT | RG_KEY_FLAG_NEEDSYNC);
	}
	return key->commentSize=0;
}







/*

int keyGetInteger(Key *key, RG_DWORD *returnedInt) {
	if (!key || !keyIsInitialized(key))
		return errno=RG_KEY_RET_UNINITIALIZED;

	if (!key->data)
		return errno=RG_KEY_RET_NODATA;

	if (key->type > RG_KEY_TYPE_DOUBLE)
		return errno=RG_KEY_RET_TYPEMISMATCH;

	switch (key->type) {
		case RG_KEY_TYPE_DOUBLE:
			*returnedInt=floor(*(double *)key->data);
			break;
		case RG_KEY_TYPE_DWORD:
			*returnedInt=*(long long int *)key->data;
			break;
	}

	return 0;
}





size_t keySetInteger(Key *key, RG_DWORD newInt) {
	size_t ret;
	char number[60];

	ret=sprintf(number,"%d",newInt);

	if ((ret=keySetRaw(key,number,ret+1))<0) {
		return ret;
	}

	key->type=RG_KEY_TYPE_STRING;

	return ret;
}
*/


/*

int keyGetDouble(Key *key, double *returnedDouble) {
	if (!key || !keyIsInitialized(key))
		return errno=RG_KEY_RET_UNINITIALIZED;

	if (!key->data)
		return errno=RG_KEY_RET_NODATA;

	if (key->type > RG_KEY_TYPE_DOUBLE)
		return errno=RG_KEY_RET_TYPEMISMATCH;

	switch (key->type) {
		case RG_KEY_TYPE_DOUBLE:
			*returnedDouble=*(double *)key->data;
			break;
		case RG_KEY_TYPE_DWORD:
			*returnedDouble=*(long long int *)key->data;
			break;
	}

	return 0;
}



size_t keySetDouble(Key *key, double newDouble) {
	size_t ret;

	if ((ret=keySetRaw(key,&newDouble,sizeof(double)))<0) {
		return ret;
	}

	key->type=RG_KEY_TYPE_DOUBLE;

	return ret;
}
*/



size_t keySetRaw(Key *key, void *newBinary, size_t dataSize) {
	if (!key) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	if (!dataSize || !newBinary) {
		if (key->data) {
			free(key->data);
			key->data=0;
		}
		key->flags &= ~(RG_KEY_FLAG_HASDATA);
		key->flags |= RG_KEY_FLAG_NEEDSYNC;
		return 0;
	}

	key->dataSize=dataSize;
	if (key->data) key->data=realloc(key->data,key->dataSize);
	else key->data=malloc(key->dataSize);

	if (!key->data) return 0;

	memcpy(key->data,newBinary,key->dataSize);
	key->flags |= RG_KEY_FLAG_HASDATA | RG_KEY_FLAG_NEEDSYNC;
	return key->dataSize;
}





size_t keyGetString(Key *key, char *returnedString, size_t maxSize) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->data) {
		*returnedString=0;
		errno=RG_KEY_RET_NODATA;
		return 0;
	}
	
	if (key->dataSize > maxSize) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	}
	
	if (key->type < RG_KEY_TYPE_STRING) {
		errno=RG_KEY_RET_TYPEMISMATCH;
		return 0;
	}
		
	strcpy(returnedString,key->data);
	return key->dataSize;
}





size_t keySetString(Key *key, char *newString) {
	size_t ret=newString?strblen(newString):0;

	if (!newString || !ret) ret=keySetRaw(key,0,0);
	else keySetRaw(key,newString,ret);

	keySetType(key,RG_KEY_TYPE_STRING);

	return ret;
}




size_t keyGetBinary(Key *key, void *returnedBinary, size_t maxSize) {
	if (!key || !keyIsInitialized(key))
		return errno=RG_KEY_RET_UNINITIALIZED;

	if (!key->data) {
		errno=RG_KEY_RET_NODATA;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	}

	memcpy(returnedBinary,key->data,key->dataSize);
	return key->dataSize;
}




size_t keySetBinary(Key *key, void *newBinary, size_t dataSize) {
	size_t ret=keySetRaw(key,newBinary,dataSize);
	
	keySetType(key,RG_KEY_TYPE_BINARY);
	
	return ret;
}




size_t keyGetLink(Key *key, char *returnedTarget, size_t maxSize) {
	if (!key || !keyIsInitialized(key))
		return errno=RG_KEY_RET_UNINITIALIZED;

	if (!key->data) {
		errno=RG_KEY_RET_NODATA;
		return 0;
	}

	if (key->type != RG_KEY_TYPE_LINK) {
		errno=RG_KEY_RET_TYPEMISMATCH;
		return 0;
	}
	
	if (key->dataSize > maxSize) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	}
		
	strcpy(returnedTarget,key->data);
	return key->dataSize;
}





size_t keySetLink(Key *key, char *target) {
	size_t ret=target?strblen(target):0;

	if (!target || !ret) ret=keySetRaw(key,0,0);
	else keySetRaw(key,target,ret);

	keySetType(key,RG_KEY_TYPE_LINK);

	return ret;
}




int keyDup(Key *source, Key *dest) {

	/* clear everything first */
	keyClose(dest);
	
	/* Copy the struct data */
	dest=source;
	
	/* prepare to set dynamic properties */
	dest->key=
	dest->comment=
	dest->userDomain=
	dest->data=0;

	/* Set properties that need memory allocation */
	keySetName(dest,source->key);
	keySetComment(dest,source->comment);
	keySetOwner(dest,source->userDomain);
	keySetRaw(dest,source->data,source->dataSize);
	
	dest->flags=source->flags;
	
	return 0;
}




uid_t keyGetUID(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	/*if (!(key->flags & RG_KEY_FLAG_HASUID)) return RG_KEY_RET_NOCRED;*/

	return key->uid;
}





int keySetUID(Key *key, uid_t uid) {
	if (!key) return errno=RG_KEY_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->uid=uid;
	key->flags |= RG_KEY_FLAG_HASUID | RG_KEY_FLAG_NEEDSYNC;

	return 0;
}



gid_t keyGetGID(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	/*if (!(key->flags & RG_KEY_FLAG_HASGID)) return RG_KEY_RET_NOCRED;*/

	return key->gid;
}



int keySetGID(Key *key, gid_t gid) {
	if (!key) return errno=RG_KEY_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->gid=gid;
	key->flags |= RG_KEY_FLAG_HASGID | RG_KEY_FLAG_NEEDSYNC;

	return 0;
}



mode_t keyGetAccess(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	/*if (!(key->flags & RG_KEY_FLAG_HASPRM)) return RG_KEY_RET_NOCRED;*/

	return key->access;
}




int keySetAccess(Key *key, mode_t mode) {
	if (!key) return errno=RG_KEY_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->access=mode;
	key->flags |= RG_KEY_FLAG_HASPRM | RG_KEY_FLAG_NEEDSYNC;

	return 0;
}






time_t keyGetMTime(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & RG_KEY_FLAG_HASTIME)) return RG_KEY_RET_NOTIME;*/

	return key->mtime;
}



time_t keyGetATime(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & RG_KEY_FLAG_HASTIME)) return RG_KEY_RET_NOTIME;*/

	return key->atime;
}



time_t keyGetCTime(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & RG_KEY_FLAG_HASTIME)) return RG_KEY_RET_NOTIME;*/

	return key->ctime;
}



size_t keyGetParentSize(Key *key) {
	char *parentNameEnd;
	
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->key) {
		errno=RG_KEY_RET_NOKEY;
		return 0;
	}
	
	/*
		user   (size=0)
		user/parent/base
		user/parent/base/ (size=sizeof("user/parent"))
	*/
	
	parentNameEnd=rindex(key->key,RG_KEY_DELIM);
	
	if (!parentNameEnd || parentNameEnd==key->key) {
		/* handle NULL or /something */
		return 0;
	}
	
	/* handle system/parent/base/ */
	if ((parentNameEnd-key->key) == (strlen(key->key)-1)) {
		parentNameEnd--;
		while (*parentNameEnd!=RG_KEY_DELIM) parentNameEnd--;
	}

	return parentNameEnd - key->key;
}




size_t keyGetParent(Key *key, char *returnedParent, size_t maxSize) {
	size_t parentSize;
	
	parentSize=keyGetParentSize(key);
	
	if (parentSize > maxSize) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	} else strncpy(returnedParent,key->key,parentSize);

	return parentSize;
}



/*
	String generated is of the form:

	<key id="123445" uid="root" gid="root" mode="0660"
		atime="123456" ctime="123456" mtime="123456"
	
		name="system/sw/XFree/Monitor/Monitor0/Name"
		type="string">
		
		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>
	
*/


u_int32_t keyCompare(Key *key1, Key *key2) {
	u_int32_t ret=0;
	
	
	/* Compare these numeric properties */
	if (key1->uid != key2->uid)                    ret|=RG_KEY_FLAG_HASUID;
	if (key1->gid != key2->gid)                    ret|=RG_KEY_FLAG_HASGID;
	if (key1->type != key2->type)                  ret|=RG_KEY_FLAG_HASTYPE;
	if ((key1->access & (S_IRWXU|S_IRWXG|S_IRWXO)) !=
		(key2->access & (S_IRWXU|S_IRWXG|S_IRWXO))) ret|=RG_KEY_FLAG_HASPRM;
	
	/* Compare these string properties.
	   A lot of decisions because strcmp can't handle NULL pointers */
	if (key1->key && key2->key) {
		if (strcmp(key1->key,key2->key))            ret|=RG_KEY_FLAG_HASKEY;
	} else {
		if (key1->key)                              ret|=RG_KEY_FLAG_HASKEY;
		else if (key2->key)                         ret|=RG_KEY_FLAG_HASKEY;
	} 
	
	if (key1->comment && key2->comment) {
		if (strcmp(key1->comment,key2->comment))    ret|=RG_KEY_FLAG_HASCOMMENT;
	} else {
		if (key1->comment)                          ret|=RG_KEY_FLAG_HASCOMMENT;
		else if (key2->comment)                     ret|=RG_KEY_FLAG_HASCOMMENT;
	}
	
	if (key1->userDomain && key2->userDomain) {
		if (strcmp(key1->userDomain,key2->userDomain)) ret|=RG_KEY_FLAG_HASDOMAIN;
	} else {
		if (key1->userDomain)                       ret|=RG_KEY_FLAG_HASDOMAIN;
		else if (key2->comment)                     ret|=RG_KEY_FLAG_HASDOMAIN;
	}
	
	
	/* Compare data */
	if (memcmp(key1->data,key2->data,
			(key1->dataSize<=key2->dataSize?key1->dataSize:key2->dataSize)))
		ret|=RG_KEY_FLAG_HASDATA;
	
	return ret;
}


/**
    Compare ks1 against ks2.
    A key (by full name) that is present on ks1 and ks2, and has something different, will be transfered from ks2 to ks1, and ks1's version deleted.
    Keys that are in ks1, but aren't in ks2 will be trasnsfered from ks1 to removed.
    Keys that are keyCompare() equal in ks1 and ks2 will be deleted from ks2.
    Keys that are available in ks2 but don't exist in ks1 will be transfered to ks1.

    In the end, ks1 will have all the keys that matter, and ks2 will be empty.
    After ksCompare(), you should: 
    - call registrySetKeys(ks1) to commit all changed keys
    - registryRemove() for all keys in the removed KeySet
    - free(ks2)
*/
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed) {
	int flagRemoved=1;
	Key *ks1Cursor=0;
	Key *ks2Cursor=0;
	
	Key *ks1PrevCursor=0;
	
	ks1Cursor=ks1->start;
	while (ks1Cursor) {
		Key *ks2PrevCursor=0;
		flagRemoved=1;
		
		for (ks2Cursor=ks2->start; ks2Cursor; ks2Cursor=ks2Cursor->next) {
			u_int32_t flags=keyCompare(ks1Cursor,ks2Cursor);
			
			if (!(flags & (RG_KEY_FLAG_HASKEY | RG_KEY_FLAG_HASDOMAIN))) {
				/* Comparing fullname-equal keys */
				flagRemoved=0; /* key was not removed */
					
				/* First remove from ks2 */
				if (ks2PrevCursor) ks2PrevCursor->next=ks2Cursor->next;
				else ks2->start=ks2Cursor->next;
				if (ks2->end==ks2Cursor) ks2->end=ks2PrevCursor;
				ks2->size--;
					
				if (flags) {
					/* keys are different. Transfer to ks1. */
					
					/* Put in ks1 */
					if (ks1PrevCursor) ks1PrevCursor->next=ks2Cursor;
					else ks1->start=ks2Cursor;
					if (ks1->end==ks1Cursor) ks1->end=ks2Cursor;
					ks2Cursor->next=ks1Cursor->next;
					
					/* delete old version */
					keyClose(ks1Cursor); free(ks1Cursor);
					
					/* Reset pointers */
					ks1Cursor=ks2Cursor;
				} else {
					/* Keys are identical. Delete ks2's key. */

					/* Delete ks2Cusrsor */
					keyClose(ks2Cursor);
					free(ks2Cursor);
				}
				/* Don't need to walk through ks2 anymore */
				break;
			}
			ks2PrevCursor=ks2Cursor;
			
		} /* ks2 iteration */
		
		if (flagRemoved) {
			/* This ks1 key was not found in ks2 */
			/* Transfer it from ks1 to removed */
			
			/* Remove from ks1 */
			if (ks1PrevCursor) ks1PrevCursor->next=ks1Cursor->next;
			else ks1->start=ks1Cursor->next;
			if (ks1->end==ks1Cursor) ks1->end=ks1PrevCursor;
			ks1->size--;

			/* Append to removed */
			ksAppend(removed,ks1Cursor);
			
			/* Reset pointers */
			if (ks1PrevCursor) ks1Cursor=ks1PrevCursor->next;
			else ks1Cursor=ks1->start;
		} else {
			ks1PrevCursor=ks1Cursor;
			ks1Cursor=ks1Cursor->next;
		}
	} /* ks1 iteration */
	
	/* Now transfer all remaining ks2 keys to ks1 */
	ksAppendKeys(ks1,ks2);
	
	return 0;
}


size_t keyToStream(Key *key, FILE* stream, unsigned long options) {
	size_t written=0;
	char buffer[800];
	struct passwd *pwd=0;
	struct group *grp=0;


	if (!key || !keyIsInitialized(key) || !key->key) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	keyGetFullName(key,buffer,sizeof(buffer));
	
	if (!(options & RG_O_NUMBERS)) {
		pwd=getpwuid(keyGetUID(key));
		grp=getgrgid(keyGetGID(key));
	}

	/* Write key name */
	written+=fprintf(stream,"<key name=\"%s\"", buffer);

	if (options & RG_O_CONDENSED) written+=fprintf(stream," ");
	else written+=fprintf(stream,"\n     ");

	
	
		
	/* Key type */
	if (options & RG_O_NUMBERS) {
		written+=fprintf(stream,"type=\"%d\"", keyGetType(key));
	} else {
		u_int8_t type=keyGetType(key);
		buffer[0]=0;
		
		switch (type) {
			case RG_KEY_TYPE_STRING:
				strcpy(buffer,"string");
				break;
			case RG_KEY_TYPE_BINARY:
				strcpy(buffer,"binary");
				break;
			case RG_KEY_TYPE_LINK:
				strcpy(buffer,"link");
				break;
			case RG_KEY_TYPE_DIR:
				strcpy(buffer,"directory");
				break;
		}
		if (buffer[0]) written+=fprintf(stream,"type=\"%s\"", buffer);
		else written+=fprintf(stream,"type=\"%d\"", type);
	}
	
	
	
	/* UID, GID, mode */
	if (pwd) written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
	else  written+=fprintf(stream,   " uid=\"%d\"",keyGetUID(key));
	
	if (grp) written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
	else  written+=fprintf(stream,   " gid=\"%d\"",keyGetGID(key));
	
	written+=fprintf(stream," mode=\"0%o\">",
		keyGetAccess(key) & (S_IRWXU|S_IRWXG|S_IRWXO));

	
	
	if (!(options & RG_O_CONDENSED) && (key->data || key->comment))
		written+=fprintf(stream,"\n\n     ");
	
	if (key->data)
		written+=
			fprintf(stream,"<value><![CDATA[%s]]></value>",(char *)key->data);



	if (!(options & RG_O_CONDENSED)) {
		written+=fprintf(stream,"\n");
		if (key->comment) written+=fprintf(stream,"     ");
	}
	
	if (key->comment) {
		written+=fprintf(stream,"<comment><![CDATA[%s]]</comment>", key->comment);
		if (!(options & RG_O_CONDENSED))
			written+=fprintf(stream,"\n");
	}

	written+=fprintf(stream,"</key>");
	
	if (!(options & RG_O_CONDENSED))
		written+=fprintf(stream,"\n\n\n\n\n\n");
	
	return written;
}



size_t ksToStream(KeySet *ks, FILE* stream, unsigned long options) {
	size_t written=0;
	Key *key=0;
	
	if (options & RG_O_XMLHEADERS) {
		written+=fprintf(stream,"<?xml version=\"1.0\" encoding=\"%s\"?>\n",
			nl_langinfo(CODESET));
		written+=fprintf(stream,
			"<!DOCTYPE keyset PUBLIC \"-//Avi Alkalay//DTD Registry 0.1.0//EN\" \"http://registry.sf.net/dtd/registry.dtd\">\n\n\n");
		written+=fprintf(stream,
			"<!-- Generated by the Linux Registry API. Total of %d keys. -->\n\n\n\n",ks->size);
	}
	
	written+=fprintf(stream,"<keyset>\n\n\n");
	
	for (key=ks->start; key; key=key->next)
		written+=keyToStream(key,stream,options);
	
	written+=fprintf(stream,"</keyset>\n");
	return written;
}




size_t keyToString(Key *key, char *returned, size_t maxSize) {
	char *cursor=returned;

	if (!key || !keyIsInitialized(key) || !key->key) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	cursor+=snprintf(cursor,maxSize,"[");
	cursor+=keyGetFullName(key,cursor,maxSize-(cursor-returned))-1;
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"]");

	if (key->comment) {
		cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nComment=");
		cursor+=keyGetComment(key,cursor,maxSize-(cursor-returned));
	}
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nType=");
/*
	if (key->flags & RG_KEY_FLAG_HASGROUP) {
		cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nGroup=");
		keyGetGroup(key,cursor,maxSize-(cursor-returned));
		cursor+=key->groupSize-1;
	}*/

	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nUID=%d",key->uid);
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nGID=%d",key->gid);
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nAccess=%d",key->access);
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nLast Modification Time=%d",(unsigned)key->mtime);
	cursor+=snprintf(cursor,maxSize-(cursor-returned),"\nValue=");
	cursor+=keyGetString(key,cursor,maxSize-(cursor-returned))-1;
	*cursor='\n'; cursor++;
	*cursor='\n'; cursor++;
	*cursor=0;

	return cursor-returned;
}



int ksInit(KeySet *ks) {
	ks->start=ks->end=ks->cursor=0;
	ks->size=0;
	
	return 0;
}


int ksClose(KeySet *ks) {
	if (ks->size) {
		Key *cursor=ks->start;
		while (ks->size) {
			Key *destroyer=cursor;
			cursor=cursor->next;
			keyClose(destroyer);
			free(destroyer);
			--ks->size;
		}
	}
	ks->start=ks->end=ks->cursor;
	return 0;
}


Key *ksNext(KeySet *ks) {
	if (!ks->cursor) ks->cursor=ks->start;
	else ks->cursor=ks->cursor->next;
	
	return ks->cursor;
}


int ksRewind(KeySet *ks) {
	ks->cursor=0;
	return 0;
}

unsigned int ksInsert(KeySet *ks, Key *toInsert) {
	toInsert->next=ks->start;
	ks->start=toInsert;
	if (!ks->end) ks->end=toInsert;
	return ++ks->size;
}


unsigned int ksAppend(KeySet *ks, Key *toAppend) {
	toAppend->next=0;
	if (ks->end) ks->end->next=toAppend;
	if (!ks->start) ks->start=toAppend;
	ks->end=toAppend;
	return ++ks->size;
}


unsigned int ksInsertKeys(KeySet *ks, KeySet *toInsert) {
	if (toInsert->size) {
		toInsert->end->next=ks->start;
		ks->start=toInsert->start;

		ks->size+=toInsert->size;
		
		/* Invalidate the old KeySet */
		toInsert->start=toInsert->end=0;
		toInsert->size=0;
	}
	return ks->size;
}



unsigned int ksAppendKeys(KeySet *ks, KeySet *toAppend) {
	if (toAppend->size) {
		if (ks->end) {
			ks->end->next=toAppend->start;
			ks->end=toAppend->end;
		} else {
			ks->end=toAppend->end;
			ks->start=toAppend->start;
		}

		ks->size+=toAppend->size;
		
		/* Invalidate the old KeySet */
		toAppend->start=toAppend->end=0;
		toAppend->size=0;
	}
	return ks->size;
}





/*
size_t keyGetSerializedSizeWithoutName(Key *key) {
	return sizeof(KeyInfo)+
		key->dataSize+
		key->groupSize+
		key->commentSize;
}
*/

/*
size_t keyGetSerializedSize(Key *key) {
	size_t totalSize;

	totalSize=(key->key)+1+keyGetSerializedSizeWithoutName(key);
	if (key->userDomain) totalSize+=strlen(key->userDomain)+1;
	return totalSize;
}*/




// int keyUnserialize(Key *key,void *buffer) {
// 	void *cursor=buffer;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	keySetName(key,cursor);
// 	cursor+=strlen(cursor)+1;
//
// 	return keyUnserializeWithoutName(key,cursor);
// }




// int keyUnserializeWithoutName(Key *key,void *buffer) {
// 	void *cursor=buffer;
//
// 	/* A valid serialized key has the following layout
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	key->metaInfo=*(KeyInfo *)cursor;
// 	cursor+=sizeof(KeyInfo);
//
// 	keySetGroup(key,cursor);
// 	cursor+=key->groupSize;
//
// 	keySetComment(key,cursor);
// 	cursor+=key->commentSize;
//
// 	keySetRaw(key,cursor,key->dataSize);
//
// 	return RG_KEY_RET_OK;
// }






// int keySerialize(Key *key,void *buffer, size_t maxSize) {
// 	void *cursor=buffer;
// 	size_t keySize;
//
// 	if (!key) return RG_KEY_RET_NULLKEY;
// 	if (!keyIsInitialized(key)) return RG_KEY_RET_UNINITIALIZED;
// 	if (!(key->flags & RG_KEY_FLAG_HASKEY)) return RG_KEY_RET_NOKEY;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated full key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	/* The key name */
// 	keyGetFullName(key,cursor,maxSize);
// 	keySize=keyGetFullNameSize(key);
// 	cursor+=keySize+1;
//
// 	return keySerializeWithoutName(key,cursor,maxSize-(cursor-buffer));
// }








// int keySerializeWithoutName(Key *key,void *buffer, size_t maxSize) {
// 	void *cursor=buffer;
//
// 	if (!key) return RG_KEY_RET_NULLKEY;
// 	if (!keyIsInitialized(key)) return RG_KEY_RET_UNINITIALIZED;
// 	if (!(key->flags & RG_KEY_FLAG_HASKEY)) return RG_KEY_RET_NOKEY;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	/* The KeyInfo whole struct */
// 	memcpy(cursor, &key->metaInfo, sizeof(KeyInfo));
// 	cursor+=sizeof(KeyInfo);
//
// 	/* The group name */
// 	if (key->group)
// 		memcpy(cursor, key->group, key->groupSize);
// 	else *(char *)cursor=0;
// 	if (key->groupSize<=1) cursor++;
// 	else cursor+=key->groupSize;
//
//
// 	/* The description */
// 	if (key->comment) memcpy(cursor, key->comment, key->commentSize);
// 	else *(char *)cursor=0;
// 	if (key->commentSize<=1) cursor++;
// 	else cursor+=key->commentSize;
//
//
// 	/* The data */
// 	if (key->data)
// 		memcpy(cursor,key->data,key->dataSize);
//
// 	return RG_KEY_RET_OK;
// }



int keyNameIsSystem(char *keyName) {
	if (!keyName) return 0;
	if (!strlen(keyName)) return 0;

	if (!strncmp("system",keyName,sizeof("system")-1)) return 1;
	return 0;
}


int keyNameIsUser(char *keyName) {
	if (!keyName) return 0;
	if (!strlen(keyName)) return 0;

	if (!strncmp("user",keyName,sizeof("user")-1)) return 1;
	return 0;
}



int keyIsSystem(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameIsSystem(key->key);
}



int keyIsUser(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameIsUser(key->key);
}


int keyNameGetNameSpace(char *keyName) {
	if (keyNameIsSystem(keyName)) return RG_NS_SYSTEM;
	if (keyNameIsUser(keyName)) return RG_NS_USER;
	return 0;
}



int keyGetNameSpace(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameGetNameSpace(key->key);
}



int keyIsDir(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return (S_ISDIR(key->access) || (key->type==RG_KEY_TYPE_DIR));
}


int keyIsLink(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return (S_ISLNK(key->access) || (key->type==RG_KEY_TYPE_LINK));
}


/** Return the key's root name size not considering the NULL termination char */
size_t keyNameGetRootNameSize(char *keyName) {
	char *end;
	int length=strlen(keyName);

	if (!length) return 0;

	/*
		Possible situations:
		user:someuser
		user:someuser/
		user:someuser/key/name
		user:some.user/key/name
		.
		\.
		(empty)
	*/

	end=keyName;
	end=strchr(end,RG_KEY_DELIM);
	if (!end) /* Reached end of string. Root is entire key. */
		end = keyName + length;
	
	return end-keyName;
}




size_t keyGetRootNameSize(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;
	if (!key->key) return 0;

	return keyNameGetRootNameSize(key->key);
}


size_t keyGetFullRootNameSize(Key *key) {
	size_t size=0;

	if (keyIsUser(key)) {
		if (key->userDomain) size=strblen(key->userDomain);
		else size=strblen(getenv("USER"));
	}

	return size+keyNameGetRootNameSize(key->key);
}



size_t keyNameGetBaseNameSize(char *keyName) {
	char *end;
	size_t size,keySize;
	unsigned char found=0;

	if (!(keySize=strblen(keyName))) return 0;

	size=keyNameGetRootNameSize(keyName);
	if (!size || size==keySize) return 0; /* key is a root key */

	/* Possible situations left:

		system/something/basename
		system/something/basename/
	*/

	end=strrchr(keyName,RG_KEY_DELIM);
	if (*(end-1)!='\\') return keyName+keySize-(end+1);

	/* Possible situations left:

		system.something.base\.name
		system.something.basename\.
	*/

	while (!found) {
		end--;
		if (*end=='.') found=1;
	}
	return keyName+keySize-(end+1);
}




size_t keyGetBaseNameSize(Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;
	if (!key->key) return 0;

	return keyNameGetBaseNameSize(key->key);
}





size_t keyGetRootName(Key *key, char *returned, size_t maxSize) {
	size_t size;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	if (!key->key) {
		errno=RG_KEY_RET_NOKEY;
		return -1;
	}

	if (!(size=keyGetRootNameSize(key))) {
		errno=RG_KEY_RET_NOKEY;
		return -1;
	}

	if (maxSize < size) {
		errno=RG_KEY_RET_TRUNC;
		return -1;
	} else strncpy(returned,key->key,size);
	return size;
}



size_t keyGetFullRootName(Key *key, char *returned, size_t maxSize) {
	size_t size;

	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->key) {
		errno=RG_KEY_RET_NOKEY;
		return 0;
	}

	if (!(size=keyGetFullRootNameSize(key))) {
		errno=RG_KEY_RET_NOKEY;
		return 0;
	}

	if (maxSize < size) {
		errno=RG_KEY_RET_TRUNC;
		return 0;
	} else strncpy(returned,key->key,size);
	return size;
}




size_t keyGetBaseName(Key *key, char *returned, size_t maxSize) {
	size_t size;
	size_t keySize;

	if (!key) return RG_KEY_RET_NULLKEY;
	if (!keyIsInitialized(key)) return RG_KEY_RET_UNINITIALIZED;
	if (!(size=keyGetBaseNameSize(key))) return RG_KEY_RET_NOKEY;

	keySize=strblen(key->key);

	if (maxSize < size) {
		strncpy(returned,key->key+keySize-size,maxSize);
		return RG_KEY_RET_TRUNC;
	} else strncpy(returned,key->key+keySize-size,size);
	return RG_KEY_RET_OK;
}




int keySetFlag(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return -1;
	}

	key->flags|=RG_KEY_FLAG_FLAG;
	
	return 0;
}


int keyGetFlag(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=RG_KEY_RET_UNINITIALIZED;
		return 0;
	}

	return (key->flags | RG_KEY_FLAG_FLAG)?1:0;
}
