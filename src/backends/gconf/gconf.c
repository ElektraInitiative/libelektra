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



/* Subversion stuff

$Id$
$LastChangedBy$

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
 *   /apps/            <->    user/sw/
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
	KEY_GCONFTYPE_LIST_STRING=    KEY_TYPE_STRING+4,
	KEY_GCONFTYPE_LIST_INT=       KEY_TYPE_STRING+5,
	KEY_GCONFTYPE_LIST_FLOAT=     KEY_TYPE_STRING+6,
	KEY_GCONFTYPE_LIST_BOOL=      KEY_TYPE_STRING+7
};
 



Key *gEntryToKey(const GConfEntry *gentry) {
	Key *new=0;
	char buffer[500];
	char *c1=0;
	GConfValue *gvalue=0;
	GSList *list=0;
	GConfValueType listType;
	
	new=keyNew("user",KEY_SWITCH_END);
	c1=(char *)gconf_entry_get_key(gentry);
	if (!strcmp("/apps",c1) || !strncmp("/apps/",c1,strlen("/apps/"))) {
		/* convert "/apps/" to "user/sw/" */
		c1=index(c1+1,'/');
		keyAddBaseName(new,"sw");
	}
	keyAddBaseName(new,(const char *)c1);
	
	gvalue=gconf_entry_get_value(gentry);
	if (!gvalue) return new;
	
	switch (gvalue->type) {
		case GCONF_VALUE_PAIR:
		case GCONF_VALUE_SCHEMA:
			/* please stop that stupid compiler warnings ! */
			break;
		case GCONF_VALUE_INVALID:
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
			
			while (list) {
				GConfValue *val=0;
				val=(GConfValue *)list->data;
				if (prev) strcat(buffer,",");
				
				switch (listType) {
					case GCONF_VALUE_SCHEMA:
						break;
					case GCONF_VALUE_INVALID:
					case GCONF_VALUE_STRING:
						keySetType(new,KEY_GCONFTYPE_LIST_STRING);
						strcat(buffer,gconf_value_get_string(val));
						break;
					case GCONF_VALUE_INT:
						keySetType(new,KEY_GCONFTYPE_LIST_INT);
						sprintf(buffer+strblen(buffer)-1,"%d",
							gconf_value_get_int(val));
						break;
					case GCONF_VALUE_FLOAT:
						keySetType(new,KEY_GCONFTYPE_LIST_FLOAT);
						sprintf(buffer+strblen(buffer)-1,"%g",
							gconf_value_get_float(val));
						break;
					case GCONF_VALUE_BOOL:
						keySetType(new,KEY_GCONFTYPE_LIST_BOOL);
						if (gconf_value_get_bool(val)) strcat(buffer,"1");
						else strcat(buffer,"0");
						break;
					case GCONF_VALUE_LIST:
					case GCONF_VALUE_PAIR:
						/* please stop these stupid compiler warnings ! */
						break;
				}
				prev=1;
				list=list->next;
			}
			
			strcat(buffer,"]");
			keySetRaw(new,buffer,strblen(buffer));
			break;
		/* TODO: car+cdr (pair). */
	}
	
	/* TODO: metainfo */
	
	return new;
}




GConfEntry *keyToGEntry(const Key *key) {
	GConfEntry *gentry=0;
	GConfValue *gval=0;
	char *keyName=0;
	size_t size=0;
	GSList *list=0;
	char *cursor=0;
	char *previous=0;
	char *end=0;
	char element[300];
	GConfValue *valelement=0;
	
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
		
		if (!strcmp("user/sw",keyName) || !strncmp("user/sw/",keyName,sizeof("user/sw/")-1)) {
			/* convert to "/apps" */
			strcpy(keyName,"/apps");
			start=keyName+sizeof("user/sw")-1;
		} else {
			start=keyName+sizeof("user")-1;
			keyName[0]=0;
		}
		
		strcat(keyName,start);
		
		/* remove multiple '/' in the begining */
		start=keyName;
		while (start[0]==RG_KEY_DELIM && start[1]==RG_KEY_DELIM)
			start++;
		
		if (start != keyName) strcpy(keyName,start);
	}
	
	/* prepare value */
	switch (key->type) {
		case KEY_GCONFTYPE_STRING:
			gval=gconf_value_new(GCONF_VALUE_STRING);
			gconf_value_set_string(gval,(char *)key->data);
			break;
		case KEY_GCONFTYPE_INT:  
			gval=gconf_value_new(GCONF_VALUE_INT);
			gconf_value_set_int(gval,atoi((char *)key->data));
			break;
		case KEY_GCONFTYPE_FLOAT:  
			gval=gconf_value_new(GCONF_VALUE_FLOAT);
			gconf_value_set_float(gval,atof((char *)key->data));
			break;
		case KEY_GCONFTYPE_BOOL:  
			gval=gconf_value_new(GCONF_VALUE_BOOL);
			gconf_value_set_bool(gval,atoi((char *)key->data));
			break;
		case KEY_GCONFTYPE_LIST_INT:
		case KEY_GCONFTYPE_LIST_FLOAT:
		case KEY_GCONFTYPE_LIST_BOOL:
		case KEY_GCONFTYPE_LIST_STRING:
			gval=gconf_value_new(GCONF_VALUE_LIST);
			
			end=rindex(key->data,']');
			
			previous=key->data+1; // jump '['
			while ((cursor=index(previous,','))) {
				strncpy(element,previous,cursor-previous);
				element[cursor-previous]=0;
				
				switch (key->type) {
					case KEY_GCONFTYPE_LIST_INT:
						valelement=gconf_value_new(GCONF_VALUE_INT);
						gconf_value_set_int(valelement,atoi(element));
						break;
					case KEY_GCONFTYPE_LIST_BOOL:
						valelement=gconf_value_new(GCONF_VALUE_BOOL);
						gconf_value_set_bool(valelement,atoi(element));
						break;
					case KEY_GCONFTYPE_LIST_FLOAT:
						valelement=gconf_value_new(GCONF_VALUE_FLOAT);
						gconf_value_set_float(valelement,atof(element));
						break;
					case KEY_GCONFTYPE_LIST_STRING:
						valelement=gconf_value_new(GCONF_VALUE_STRING);
						gconf_value_set_string(valelement,element);
						break;
				}
				
				list=g_slist_append(list,valelement);
				previous=cursor+1;
			}
			
			strncpy(element,previous,(char *)key->data+key->dataSize-2-previous);
			element[(char *)key->data+key->dataSize-2-previous]=0;
			
			switch (key->type) {
				case KEY_GCONFTYPE_LIST_INT:
					valelement=gconf_value_new(GCONF_VALUE_INT);
					gconf_value_set_int(valelement,atoi(element));
					gconf_value_set_list_type(gval,GCONF_VALUE_INT);
					break;
				case KEY_GCONFTYPE_LIST_BOOL:
					valelement=gconf_value_new(GCONF_VALUE_BOOL);
					gconf_value_set_bool(valelement,atoi(element));
					gconf_value_set_list_type(gval,GCONF_VALUE_BOOL);
					break;
				case KEY_GCONFTYPE_LIST_FLOAT:
					valelement=gconf_value_new(GCONF_VALUE_FLOAT);
					gconf_value_set_float(valelement,atof(element));
					gconf_value_set_list_type(gval,GCONF_VALUE_FLOAT);
					break;
				case KEY_GCONFTYPE_LIST_STRING:
					valelement=gconf_value_new(GCONF_VALUE_STRING);
					gconf_value_set_string(valelement,element);
					gconf_value_set_list_type(gval,GCONF_VALUE_STRING);
					break;
			}
			g_slist_append(list,valelement);
			
			gconf_value_set_list_nocopy(gval,list);
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



int kdbGetKey_gconf(Key *key) {
	GConfEntry *gentry=0, *gkey;
	Key *dup;
	GError *gerr=0;
	int rc=0;
	
	gkey=keyToGEntry(key);
	if (!gkey) {
		errno=ENOENT;
		return -1;
	}
	
	gentry=gconf_engine_get_entry(gconf,gconf_entry_get_key(gkey),0,1,&gerr);
	
	g_free(gkey->key);
	if (gkey->value) gconf_value_free(gkey->value);
	g_free(gkey);
	
	if (gerr != 0) {
		errno=gerr->code;
		g_error_free(gerr);
		
		g_free(gentry->key);
		if (gentry->value) gconf_value_free(gentry->value);
		g_free(gentry);
		
		return errno;
	}
	
	if (gentry) {
		dup=gEntryToKey(gentry);
		keyDup(dup,key);
		keyDel(dup);
		rc=0;
	} else rc=1;
	
	g_free(gentry->key);
	if (gentry->value) gconf_value_free(gentry->value);
	g_free(gentry);
	
	return rc;
}



/**
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_gconf(Key *key) {
	GConfEntry *gentry=0;
	GError *err=0;
	
	if (key->type == KEY_TYPE_DIR) return 0;
	
	gentry=keyToGEntry(key);
	gconf_engine_set(gconf,gconf_entry_get_key(gentry),gconf_entry_get_value(gentry),&err);
	gconf_entry_free(gentry);
	
	if (err != 0) {
		errno=err->code;
		g_error_free(err);
		return errno;
	}
	
	return 0; /* success */
}



/**
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_gconf(const Key *key) {
	GError *err=0;
	GConfEntry *gentry;
	gboolean rc=0;
	
	if (!key || !key->key) {
		errno=KDB_RET_NULLKEY;
		return 1; /* errno is propagated */
	}
	
	gentry=keyToGEntry(key);
	rc=gconf_engine_unset(gconf,gentry->key,&err);
	gconf_entry_free(gentry);
	
	
	/* GConf would not tell us anything about a failure to remove
	 * a directory, so this test block is almost useless for this
	 * purpose.
	 * In fact, it looks like there is no way to remove a directory
	 * in GConf. What a shame.
	 */ 
	if (!rc) {
		if (err != 0) {
			errno=err->code;
			g_error_free(err);
		}
		
		return 1;
	}
	
	return 0;  /* success */
}




int kdbGetKeyChildKeys_gconf(const Key *parentKey, KeySet *returned, unsigned long options) {
	GConfEntry *entry=keyToGEntry(parentKey);
	GError *gerr=0;
	GSList *dirs=0,*entries=0,*current=0;
	
	if (!entry) {
		errno=ENOENT;
		return -1;
	}
	
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
		Key *toAppend=0;
		GConfEntry entry;
		
		entry.key=dirs->data;
		entry.value=0;
		
		toAppend=gEntryToKey(&entry);
		
		keySetType(toAppend,KEY_TYPE_DIR);
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



int kdbRename_gconf(Key *key, const char *newName) {
	int rc;
	
	if ((rc=kdbGetKey_gconf(key))) return rc;
	if ((rc=kdbRemoveKey_gconf(key))) return rc;
	if (! keySetName(key,newName)) return 1;
	return kdbSetKey_gconf(key);
}




KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_gconf,
		KDB_BE_CLOSE,          &kdbClose_gconf,
		KDB_BE_GETKEY,         &kdbGetKey_gconf,
		KDB_BE_SETKEY,         &kdbSetKey_gconf,
		KDB_BE_STATKEY,        &kdbGetKey_gconf,
		KDB_BE_RENAME,         &kdbRename_gconf,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_gconf,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_gconf,
		/* set to default implementation: */
		KDB_BE_MONITORKEY,     &kdbMonitorKey_default,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_default,
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}
