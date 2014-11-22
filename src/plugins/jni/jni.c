/***************************************************************************
                     jni.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>

#include <jni.h>
#include <stdlib.h>


#include <kdbplugin.h>
#include <kdberrors.h>

// forward declarations
int elektraJniOpen(Plugin *handle, Key *errorKey);
int elektraJniClose(Plugin *handle, Key *errorKey);
int elektraJniGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJniSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJniError(Plugin *handle, KeySet *ks, Key *parentKey);

// plugin's data handle
typedef struct
{
	JNIEnv *env;
	JavaVM *jvm;
	jclass clsPlugin;
	jclass clsKey;
	jclass clsKeySet;
	jmethodID midKey; //! Java constructor for Key
	jmethodID midKeySet; //! Java constructor for KeySet
	jobject plugin;
} Data;

static void checkException(Data *data)
{
	if ((*data->env)->ExceptionCheck(data->env))
	{
		(*data->env)->ExceptionDescribe(data->env);
		// TODO: pack this in warning
		(*data->env)->ExceptionClear(data->env);
	}
}

static int call1Arg(Data *data, Key *errorKey, const char *method)
{
	jobject jerrorKey = (*data->env)->NewObject(data->env,
			data->clsKey,
			data->midKey, errorKey);
	if (jerrorKey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create errorKey");
		return -1;
	}
	checkException(data);

	jmethodID mid = (*data->env)->GetMethodID(data->env,
			data->clsPlugin,
			method, "(LElektra/Key;)I");
	if (mid== 0)
	{
		ELEKTRA_SET_ERRORF(26, errorKey, "Cannot find %s",
				method);
		return -1;
	}
	checkException(data);

	jint result = 0;
	result = (*data->env)->CallIntMethod(data->env,
			data->plugin,
			mid,
			jerrorKey
			);
	checkException(data);

	return result;
}

static int call2Arg(Data *data, KeySet *ks, Key *errorKey, const char *method)
{
	jobject jks = (*data->env)->NewObject(data->env,
			data->clsKeySet,
			data->midKeySet, ks);
	if (jks == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create ks");
		return -1;
	}
	checkException(data);

	jobject jkey = (*data->env)->NewObject(data->env,
			data->clsKey,
			data->midKey, errorKey);
	if (jkey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create key");
		return -1;
	}
	checkException(data);

	jmethodID mid = (*data->env)->GetMethodID(data->env,
			data->clsPlugin,
			method, "(LElektra/KeySet;LElektra/Key;)I");
	if (mid== 0)
	{
		ELEKTRA_SET_ERRORF(26, errorKey, "Cannot find %s",
				method);
		return -1;
	}
	checkException(data);

	jint result = 0;
	result = (*data->env)->CallIntMethod(data->env,
			data->plugin,
			mid,
			jks,
			jkey
			);
	checkException(data);

	return result;
}

int elektraJniOpen(Plugin *handle, Key *errorKey)
{
	Data *data = malloc(sizeof(Data));

	JavaVMInitArgs vmArgs;
	JavaVMOption options[2];
	options[0].optionString = "-Djava.class.path=.:/usr/share/java/jna.jar:/usr/lib/java:/home/markus/Projekte/Elektra/libelektra/src/bindings/jna";
	// options[0].optionString = "-Djava.class.path=.:/usr/share/java/jna-3.2.7.jar:/usr/lib/java:/home/markus/Projekte/Elektra/libelektra/src/bindings/jna";
	options[1].optionString = "-verbose:gc,class,jni";
	vmArgs.version = JNI_VERSION_1_8;
	vmArgs.nOptions = 2;
	vmArgs.options = options;
	vmArgs.ignoreUnrecognized = JNI_FALSE;

	jint res = JNI_CreateJavaVM(&data->jvm,
			(void**)&data->env,
			(void**)&vmArgs);
	if (res < 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create Java VM");
		return -1;
	}

	const char *classname = "Elektra/PluginDemo";
	data->clsPlugin = (*data->env)->FindClass(data->env, classname);
	if (data->clsPlugin == 0)
	{
		ELEKTRA_SET_ERRORF(26, errorKey,
				"Cannot find class %s", classname);
		return -1;
	}

	data->clsKey = (*data->env)->FindClass(data->env, "Elektra/Key");
	if (data->clsKey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find class Key");
		return -1;
	}

	data->clsKeySet = (*data->env)->FindClass(data->env, "Elektra/KeySet");
	if (data->clsKeySet == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find class KeySet");
		return -1;
	}

	data->midKey = (*data->env)->GetMethodID(data->env, data->clsKey,
			"<init>", "(J)V");
	if (data->midKey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of Key");
		return -1;
	}
	checkException(data);

	data->midKeySet = (*data->env)->GetMethodID(data->env, data->clsKeySet,
			"<init>", "(J)V");
	if (data->midKeySet == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of KeySet");
		return -1;
	}
	checkException(data);

	jmethodID midPluginConstructor = (*data->env)->GetMethodID(
			data->env, data->clsPlugin,
			"<init>", "()V");
	if (midPluginConstructor == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of plugin");
		return -1;
	}
	checkException(data);

	data->plugin = (*data->env)->NewObject(data->env,
			data->clsPlugin,
			midPluginConstructor);
	if (data->plugin == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create plugin");
		return -1;
	}
	checkException(data);

	elektraPluginSetData(handle, data);

	return call1Arg(data, errorKey, "open");
}

int elektraJniClose(Plugin *handle, Key *errorKey)
{
	Data *data = elektraPluginGetData(handle);
	int ret = call1Arg(data, errorKey, "close");

	(*data->jvm)->DestroyJavaVM(data->jvm);
	free(data);

	return ret;
}

int elektraJniGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/jni"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/jni",
			KEY_VALUE, "jni plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/jni/exports", KEY_END),
		keyNew ("system/elektra/modules/jni/exports/open",
			KEY_FUNC, elektraJniOpen, KEY_END),
		keyNew ("system/elektra/modules/jni/exports/close",
			KEY_FUNC, elektraJniClose, KEY_END),
		keyNew ("system/elektra/modules/jni/exports/get",
			KEY_FUNC, elektraJniGet, KEY_END),
		keyNew ("system/elektra/modules/jni/exports/set",
			KEY_FUNC, elektraJniSet, KEY_END),
		keyNew ("system/elektra/modules/jni/exports/error",
			KEY_FUNC, elektraJniError, KEY_END),
#include ELEKTRA_README(jni)
		keyNew ("system/elektra/modules/jni/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
	}
	/* get all keys */
	Data *data = elektraPluginGetData(handle);
	call2Arg(data, returned, parentKey, "get");

	return 1; /* success */
}

int elektraJniSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Data *data = elektraPluginGetData(handle);
	return call2Arg(data, returned, parentKey, "set");
}

int elektraJniError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Data *data = elektraPluginGetData(handle);
	return call2Arg(data, returned, parentKey, "error");
}

Plugin *ELEKTRA_PLUGIN_EXPORT(jni)
{
	return elektraPluginExport("jni",
		ELEKTRA_PLUGIN_OPEN,	&elektraJniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraJniClose,
		ELEKTRA_PLUGIN_GET,	&elektraJniGet,
		ELEKTRA_PLUGIN_SET,	&elektraJniSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraJniError,
		ELEKTRA_PLUGIN_END);
}

