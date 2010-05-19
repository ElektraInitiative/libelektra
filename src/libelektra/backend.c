/***************************************************************************
          backend.c  -  Everything related to a backend
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


/**Builds a backend out of the configuration supplied
 * from:
 *
@verbatim
system/elektra/mountpoints/<name>
@endverbatim
 *
 * The root key must be like the above example. You do
 * not need to rewind the keyset. But every key must be
 * below the root key.
 *
 * The internal consistency will be checked in this
 * function. If necessary parts are missing, like
 * no plugins, they cant be loaded or similar 0
 * will be returned.
 *
 * ksCut() is perfectly suitable for cutting out the
 * configuration like needed.
 *
 * @return a pointer to a freshly allocated backend
 * @return 0 if it did not work, the elektra_config then
 *         has the error information.
 * @ingroup backend
 */
Backend* backendOpen(KeySet *elektra_config)
{
	Key * cur;
	Key * root;
	ksRewind(elektra_config);

	root = ksNext (elektra_config);

	Backend *backend = kdbiCalloc(sizeof(struct _Backend));

	while ((cur = ksNext(elektra_config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			// direct below root key
			KeySet *cut = ksCut (elektra_config, cur);
			if (!strcmp(keyBaseName(cur), "getplugins"))
			{
				if (processPlugins(backend->getplugins, cut) == -1)
				{
#if DEBUG
					printf ("Processing Plugins failed\n");
#endif
					backendClose(backend);
					return 0;
				}
			}
		}
		// handle->mountpoint=keyNew(mountpoint,KEY_VALUE,backendname,0);
	}

	return 0;
}

void backendClose(Backend *backend)
{
	for (int i=0; i<10; ++i)
	{
		pluginClose(backend->setplugins[i]);
		pluginClose(backend->getplugins[i]);
	}
	kdbiFree (backend);
}
