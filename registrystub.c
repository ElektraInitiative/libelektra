/***************************************************************************
            registrystub.c  -  Wrapper for backwards compatibility
                             -------------------
    begin                : Tue Aug 24 2003
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



/***************************************************************************
 *                                                                         *
 * This is a wrapper to let old binaries (looking for registry*() methods  *
 * to be able to find them, since they changed to kdb*().                  *
 *                                                                         *
 * This file will disapar soon                                             *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id: registrystub.c 51 2004-08-15 13:39:13Z aviram $
$LastChangedBy: aviram $

*/


#include "kdb.h"


int registryOpen() {
	return kdbOpen();
}

int registryClose() {
	return kdbClose();
}

int registryGetValue(char *keyname, char *returned,size_t maxSize) {
	return kdbGetValue(keyname, returned, maxSize);
}


int registryGetKeyByParent(char *parentName, char *baseName, Key *returned) {
	return kdbGetKeyByParent(parentName, baseName, returned);
}


int registryGetKeyByParentKey(Key *parent, char *baseName, Key *returned) {
	return kdbGetKeyByParentKey(parent, baseName, returned);
}



int registryGetValueByParent(char *parentName, char *baseName, char *returned, size_t maxSize) {
	return kdbGetValueByParent(parentName,baseName, returned, maxSize);
}


int registrySetValue(char *keyname, char *value) {
	return kdbSetValue(keyname, value);
}


int registrySetValueByParent(char *parentName, char *baseName, char *value) {
	return kdbSetValueByParent(parentName, baseName, value);
}

int registryRemove(char *keyName) {
	return kdbRemove(keyName);
}

int registryLink(char *oldPath,char *newKeyName) {
	return kdbLink(oldPath,newKeyName);
}


int registryGetComment(char *keyname, char *returned,size_t maxSize) {
	return kdbGetComment(keyname, returned,maxSize);
}


size_t registrySetComment(char *keyname, char *comment) {
	return kdbSetComment(keyname, comment);
}

int registryStatKey(Key *key) {
	return kdbStatKey(key);
}


int registryGetKey(Key *key) {
	return kdbGetKey(key);
}

int registrySetKey(Key *key) {
	return kdbSetKey(key);
}

int registryGetChildKeys(char *parent, KeySet *returned,unsigned long options) {
	return kdbGetChildKeys(parent, returned,options);
}

int registryGetRootKeys(KeySet *returned) {
	return kdbGetRootKeys(returned);
}

int registrySetKeys(KeySet *ks) {
	return kdbSetKeys(ks);
}

u_int32_t registryMonitorKey(Key *interest, u_int32_t diffMask,
	unsigned long iterations, unsigned usleep) {
	return kdbMonitorKey(interest, diffMask, iterations, usleep);
}


u_int32_t registryMonitorKeys(KeySet *interests, u_int32_t diffMask,
	unsigned long iterations, unsigned sleep) {
	return kdbMonitorKeys(interests, diffMask,iterations, sleep);
}

