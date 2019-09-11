/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbplugin.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>


#define BACKENDNAME "timeofday"
#define BACKENDVERSION "0.0.1"

struct _TimeofdayInfo
{
	struct timeval start;
	struct timeval last;
	int nrget;
	int nrset;
	int nrerr;
};

typedef struct _TimeofdayInfo TimeofdayInfo;

int elektraTimeofdayOpen (Plugin * handle, Key *);
int elektraTimeofdayClose (Plugin * handle, Key *);
int elektraTimeofdayGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTimeofdaySet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTimeofdayError (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;
