/***************************************************************************
          backend.c  -  Everything related to a plugin
                             -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include <kdbinternal.h>

Plugin *pluginNew()
{
	Plugin *plugin = kdbiCalloc (sizeof(struct _Plugin));

	return plugin;
}

void pluginDel(Plugin *plugin)
{
	kdbiFree(plugin);
}

/**
 * Load a plugin.
 *
 * The array of plugins must be set to 0.
 * Its length is 10.
 *
 * @return -1 on failure
 */
int processPlugins(Plugin **plugins, KeySet *config)
{
	Key *root;
	Key *cur;

	ksRewind (config);

	root = ksNext(config);

	while ((cur = ksNext(config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			// this describes a plugin!
			const char *fullname = keyBaseName(cur);
			const char *backendname;
			if (fullname[0] != '#')
			{
#if DEBUG
				printf ("Names of Plugins must start with a #\n");
#endif
				return -1;
			}
			if (fullname[1] < '0' || fullname[1] > '9')
			{
#if DEBUG
				printf ("Names of Plugins must start have the position number as second char\n");
#endif
				return -1;
			}
			backendname = &fullname[2];
			printf ("Backendname is %s\n", backendname);
		}
	}
	return 0;
}


KDB* kdbOpenPlugin(const char *backendname, const char *mountpoint, KeySet *config)
{
	KDB * handle;
	char* backend_name;

	kdbLibHandle dlhandle=0;
	typedef KDB *(*KDBBackendFactory) (void);
	KDBBackendFactory kdbBackendFactory=0;

	backend_name = malloc(sizeof("libelektra-")+strlen(backendname));

	strncpy(backend_name,"libelektra-",sizeof("libelektra-"));
	strncat(backend_name,backendname,strlen(backendname));

	dlhandle=kdbLibLoad(backend_name);
	if (dlhandle == 0) {
		/*errno=KDB_ERR_EBACKEND;*/
#if DEBUG && VERBOSE
		printf("kdbLibLoad(%s) failed\n", backend_name);
#endif
		goto err_clup; /* error */
	}

	/* load the "kdbBackendFactory" symbol from backend */
	kdbBackendFactory=(KDBBackendFactory)kdbLibSym(dlhandle, "kdbBackendFactory");
	if (kdbBackendFactory == 0) {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not kdbLibSym kdbBackendFactory for %s\n", backend_name);
#endif
		goto err_clup; /* error */
	}
	
	handle=kdbBackendFactory();
	if (handle == 0)
	{
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not call kdbBackendFactory for %s\n", backend_name);
#endif
		goto err_clup; /* error */
	}

	/* save the libloader handle for future use */
	handle->dlHandle=dlhandle;
	handle->trie	= 0;

	handle->mountpoint=keyNew(mountpoint,KEY_VALUE,backendname,0);

	/* let the backend initialize itself */
	if (handle->kdbOpen)
	{
		handle->config = config;
		if ((handle->kdbOpen(handle)) == -1)
		{
#if DEBUG && VERBOSE
			printf("kdbOpen() failed for %s\n", backend_name);
#endif
		}
	}
	else {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
			printf("No kdbOpen supplied in %s\n", backend_name);
#endif
		goto err_clup;
	}

#if DEBUG && VERBOSE
	printf("Finished loading Backend %s\n", backend_name);
#endif
	free(backend_name);
	return handle;

err_clup:
#if DEBUG
	fprintf(stderr,"Failed to load backend %s\n", backend_name);
#endif
	free(backend_name);
	return 0;
}

int kdbCloseBackend(KDB *handle)
{
	int rc=0;

	if (handle->kdbClose)
		rc=handle->kdbClose(handle);
	
	if (rc == 0) {
		kdbLibClose(handle->dlHandle);
		capDel (handle->capability);
		keyDel(handle->mountpoint);
		if (handle->config) ksDel(handle->config);
		free(handle);
	}
	
	return rc;
}
