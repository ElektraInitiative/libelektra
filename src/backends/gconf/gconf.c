/***************************************************************************
            gconf.c  -  A GConf backend for Elektra
                             -------------------
    begin                : Mon Jan 02 2005
    copyright            : (C) 2005 by Avi Alkalay
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
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libkdb.so a valid backend.                                 *
 *   Simple fill the empty _backend functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id:$
$LastChangedBy: aviram $

*/



#include <kdb.h>
#include <kdbbackend.h>
#include <stdlib.h>
#include <string.h>
#include <gconf/gconf.h>

#define BACKENDNAME "gconf"



/**Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posixsystem*/
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif




GConfEngine *gconf;


/*
 *
 *   /apps/            <->    user/apps/
 *   /system/          <->    user/system/
 *   /desktop/gnome/   <->    user/desktop/gnome/
 *
 *
 *
 */


enum KeyGConfType {
	KEY_GCONFTYPE_STRING=         KEY_TYPE_STRING,
	KEY_GCONFTYPE_INT=            KEY_TYPE_STRING+1,
	KEY_GCONFTYPE_FLOAT=          KEY_TYPE_STRING+2,
	KEY_GCONFTYPE_BOOL=           KEY_TYPE_STRING+3,
	KEY_GCONFTYPE_BOOL=           KEY_TYPE_STRING+3,
};
 



Key *gEntryToKey(const GConfEntry *gentry) {
	Key *new=0;
	char buffer[500];
	GConfValue *gvalue=0;
	GSList *list=0;
	GConfValueType listType;
	
	new=keyNew("user",KEY_SWITCH_END);
	keyAddBaseName(new,(const char *)gconf_entry_get_key(gentry));
	
	gvalue=gconf_entry_get_value(gentry);
	if (!gvalue) return new;
	
	switch (gvalue->type) {
		case GCONF_VALUE_STRING:
			keySetString(new,gconf_value_get_string(gvalue));
			break;
		case GCONF_VALUE_INT:
			sprintf(buffer,"%d",gconf_value_get_int(gvalue));
			keySetString(new,buffer);
			keySetType(new,KEY_GCONFTYPE_INT);
			break;
		case GCONF_VALUE_FLOAT:
			sprintf(buffer,"%g",gconf_value_get_float(gvalue));
			keySetString(new,buffer);
			keySetType(new,KEY_GCONFTYPE_FLOAT);
			break;
		case GCONF_VALUE_BOOL:
			sprintf(buffer,"%d",gconf_value_get_bool(gvalue));
			keySetString(new,buffer);
			keySetType(new,KEY_GCONFTYPE_BOOL);
			break;
		case GCONF_VALUE_LIST:
			listType=gconf_value_get_list_type(gvalue);
			list=gconf_value_get_list(gvalue);
			int prev=0;
			
			strcpy(buffer,"[");
			
			if (list) {
				GConfValue *val=0;
				while (list) {
					val=(GConfValue *)list->data;
					if (prev) strcat(buffer,",");
					strcat(buffer,gconf_value_get_string(val));
					
					prev=1;
					list=list->next;
				}
			}
			
			strcat(buffer,"]");
			keySetString(new,buffer);
			break;
		/* TODO: list and car+cdr. */
	}
	
	/* TODO: metainfo */
	
	return new;
}




GConfEntry *keyToGEntry(const Key *key) {
	GConfEntry *gentry=0;
	GConfValue *gval=0;
	char *keyName=0;
	size_t size=0;
	
	if (!keyIsUser(key)) return 0;
	
	/* prepare key name */
	if (strblen(key->key)<sizeof("user/")) {
		keyName=malloc(2);
		strcpy(keyName,"/");
	} else {
		char *start=0;
		
		size=keyGetNameSize(key);
		keyName=malloc(size);
		keyGetName(key,keyName,size);
		strcpy(keyName,keyName+sizeof("user")-1);
		
		/* remove multiple '/' in the begining */
		start=keyName;
		while (start[0]==RG_KEY_DELIM && start[1]==RG_KEY_DELIM)
			start++;
		
		if (start != keyName) strcpy(keyName,start);
	}
	
	/* prepare value */
	switch (keyGetType(key)) {
		case KEY_GCONFTYPE_STRING:
			gconf_value_new(GCONF_VALUE_STRING);
			gconf_value_set_string(gval,(char *)key->data);
			break;
		case KEY_GCONFTYPE_INT:  
			gval=gconf_value_new(GCONF_VALUE_INT);
			gconf_value_set_float(gval,atoi((char *)key->data));
			break;
		case KEY_GCONFTYPE_FLOAT:  
			gval=gconf_value_new(GCONF_VALUE_FLOAT);
			gconf_value_set_float(gval,atof((char *)key->data));
			break;
		case KEY_GCONFTYPE_BOOL:  
			gval=gconf_value_new(GCONF_VALUE_BOOL);
			gconf_value_set_bool(gval,atoi((char *)key->data));
			break;
	}
	
	gentry=gconf_entry_new_nocopy(keyName,gval);
	
	/* TODO: metainfo */
	
	return gentry;
}




int kdbOpen_gconf() {
	gconf=0;
	gconf=gconf_engine_get_default();
	return 0;
}




int kdbClose_gconf() {
	gconf_engine_unref(gconf);
	return 0; /* success */
}



int kdbStatKey_gconf(Key *key) {
	/* get the most possible key metainfo */
	return 0; /* success */
}


int kdbGetKey_gconf(Key *key) {
	/* fully gets a key */
	return 0; /* success */
}



/**
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_backend(Key *key) {
	/* fully sets a key */
	return 0; /* success */
}



/**
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_backend(Key *key, const char *newName) {
	/* rename a key to another name */
	return 0; /* success */
}




/**
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemove_backend(const char *keyName) {
	/* remove a key from the database */
	return 0;  /* success */
}



int kdbGetKeyChildKeys_gconf(const Key *parentKey, KeySet *returned, unsigned long options) {
	GConfEntry *entry=keyToGEntry(parentKey);
	GError *gerr=0;
	GSList *dirs=0,*entries=0,*current=0;
	
	dirs=gconf_engine_all_dirs(gconf,entry->key,&gerr);
	entries=gconf_engine_all_entries(gconf,entry->key,&gerr);

	gconf_entry_free(entry); entry=0;
	
	while (entries) {
		ksAppend(returned,gEntryToKey((GConfEntry *)entries->data));
		
		current=entries;
		entries=g_slist_remove_link(entries,current);
		
		gconf_entry_free((GConfEntry *)current->data);
		g_slist_free(current);
	}
	
	while (dirs) {
		Key *toAppend=keyNew("user",
			KEY_SWITCH_TYPE,KEY_TYPE_DIR,
			KEY_SWITCH_END);
		
		keyAddBaseName(toAppend,(char *)dirs->data);
		ksAppend(returned,toAppend);
		
		if (options & KDB_O_RECURSIVE)
			kdbGetKeyChildKeys_gconf(toAppend,returned,options & ~KDB_O_SORT);
		
		current=dirs;
		dirs=g_slist_remove_link(dirs,current);
		
		g_free(current->data);
		g_slist_free(current);
	}
	
	if (options & KDB_O_SORT) ksSort(returned);
	
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
int kdbSetKeys_backend(KeySet *ks) {
	/* set many keys */
	return 0;
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKeys_backend(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	return 0;
}



/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKey_backend(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	return 0;
}


KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_gconf,
		KDB_BE_CLOSE,          &kdbClose_gconf,
		KDB_BE_GETKEY,         &kdbGetKey_gconf,
		KDB_BE_SETKEY,         &kdbSetKey_backend,
		KDB_BE_STATKEY,        &kdbStatKey_gconf,
		KDB_BE_RENAME,         &kdbRename_backend,
		KDB_BE_REMOVEKEY,      &kdbRemove_backend,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_gconf,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_backend,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_backend,
		/* set to default implementation: */
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}
