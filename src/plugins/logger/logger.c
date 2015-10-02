/**
* \file
*
* \brief Source for logger plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <stdio.h>
#include <time.h>
#include "logger.h"

int elektraLoggerOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraLoggerClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

static void writeErrors(FILE *fp, const char *timeString, Key *parentKey)
{
	printf("writeErrors\n");
    Key *errParent = keyGetMeta(parentKey, "error");
	int nr_errors = atoi(keyString(errParent)) + 1;
	printf("got %d errors\n", nr_errors);

}
static void writeWarnings(FILE *fp, const char *timeString, Key *parentKey)
{
   	printf("writeErrors\n"); 
	Key *warningParent = keyGetMeta(parentKey, "warnings");
	int nr_warnings = atoi(keyString(warningParent)) + 1;
	printf("got %d warnings\n", nr_warnings);

}
static void writeLoggingInfo(FILE *fp, const char *timeString, KeySet *ks)
{


}
static void log(const char *fileName, KeySet *ks, Key *parentKey)
{
	printf("log()\n");
	FILE *fp = fopen(fileName, "a");
	if(!fp)
	{
		printf("error while opening %s\n", fileName);
	}
	time_t t = time(NULL);
	const char *timeString = asctime(localtime(&t));
	writeErrors(fp, timeString, parentKey);
	writeWarnings(fp, timeString, parentKey);
	writeLoggingInfo(fp, timeString, ks);
	fclose(fp);
	
}
int elektraLoggerGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/logger"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/logger",
			KEY_VALUE, "logger plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/logger/exports", KEY_END),
		keyNew ("system/elektra/modules/logger/exports/open",
			KEY_FUNC, elektraLoggerOpen, KEY_END),
		keyNew ("system/elektra/modules/logger/exports/close",
			KEY_FUNC, elektraLoggerClose, KEY_END),
		keyNew ("system/elektra/modules/logger/exports/get",
			KEY_FUNC, elektraLoggerGet, KEY_END),
		keyNew ("system/elektra/modules/logger/exports/set",
			KEY_FUNC, elektraLoggerSet, KEY_END),
		keyNew ("system/elektra/modules/logger/exports/error",
			KEY_FUNC, elektraLoggerError, KEY_END),
#include ELEKTRA_README(logger)
		keyNew ("system/elektra/modules/logger/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */
	KeySet *config = elektraPluginGetConfig(handle);
	Key *fnKey = ksLookupByName(config, "/logfile", 0);
	const char *fileName = "/tmp/elektra.log";
	if(fnKey)
		fileName = keyString(fnKey);
	log(fileName, returned, parentKey);
	return 1; /* success */
}

int elektraLoggerSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	KeySet *config = elektraPluginGetConfig(handle);
	Key *fnKey = ksLookupByName(config, "/logfile", 0);
	const char *fileName = "/tmp/elektra.log";
	if(fnKey)
		fileName = keyString(fnKey);
	log(fileName, returned, parentKey);
	return 1; /* success */
}

int elektraLoggerError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	KeySet *config = elektraPluginGetConfig(handle);
	Key *fnKey = ksLookupByName(config, "/logfile", 0);
	const char *fileName = "/tmp/elektra.log";
	if(fnKey)
		fileName = keyString(fnKey);
	log(fileName, returned, parentKey);
	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(logger)
{
	return elektraPluginExport("logger",
		ELEKTRA_PLUGIN_OPEN,	&elektraLoggerOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLoggerClose,
		ELEKTRA_PLUGIN_GET,	&elektraLoggerGet,
		ELEKTRA_PLUGIN_SET,	&elektraLoggerSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraLoggerError,
		ELEKTRA_PLUGIN_END);
}

