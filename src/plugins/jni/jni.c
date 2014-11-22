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

typedef struct
{
	JNIEnv *env;
	JavaVM *jvm;
	JavaVMInitArgs vmArgs;
	jclass cls;
	jclass clsKey;
	jclass clsKeySet;
	jmethodID midKey;
	jmethodID midKeySet;
	jmethodID midOpen;
	jmethodID midClose;
	jmethodID midGet;
	jmethodID midSet;
	jmethodID midError;
	jobject plugin;
} Data;

int elektraJniOpen(Plugin *handle, Key *errorKey);
int elektraJniClose(Plugin *handle, Key *errorKey);
int elektraJniGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJniSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJniError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(jni);

int elektraJniOpen(Plugin *handle, Key *errorKey)
{
	Data *data = malloc(sizeof(Data));

	JavaVMOption options[2];
	options[0].optionString = "-Djava.class.path=.:/usr/lib/java:/home/markus/Projekte/Elektra/libelektra/src/bindings/jna";
	options[1].optionString = "-verbose:gc,class,jni";
	data->vmArgs.version = JNI_VERSION_1_8;
	data->vmArgs.nOptions = 2;
	data->vmArgs.options = options;
	data->vmArgs.ignoreUnrecognized = JNI_FALSE;

	jint res = JNI_CreateJavaVM(&data->jvm,
			(void**)&data->env,
			(void**)&data->vmArgs);
	if (res < 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create Java VM");
		return -1;
	}

	/*
	data->cls = (*data->env)->FindClass(data->env, "Elektra/PluginDemo");
	if (data->cls == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find class PluginDemo");
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
			"<init>", "(Lcom/sun/jna/Pointer;)V");
	if (data->midKey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of Key");
		return -1;
	}

	data->midKeySet = (*data->env)->GetMethodID(data->env, data->clsKeySet,
			"<init>", "(Lcom/sun/jna/Pointer;)V");
	if (data->midKeySet == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of KeySet");
		return -1;
	}

	jobject jerrorKey = (*data->env)->NewObject(data->env,
			data->clsKey,
			data->midKey, errorKey);
	if (jerrorKey == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create errorKey");
		return -1;
	}

	jmethodID midPluginConstructor = (*data->env)->GetMethodID(
			data->env, data->cls,
			"<init>", "()V");
	if (midPluginConstructor == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find constructor of plugin");
		return -1;
	}

	data->plugin = (*data->env)->NewObject(data->env,
			data->cls,
			midPluginConstructor);
	if (data->plugin == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create plugin");
		return -1;
	}

	data->midOpen = (*data->env)->GetStaticMethodID(data->env,
			data->cls,
			"print", "()V");
	if (data->midOpen == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find open");
		return -1;
	}

	jint result = 0;
	result = (*data->env)->CallStaticIntMethod(data->env,
			data->plugin,
			data->midOpen,
			jerrorKey
			);
	printf("After open()\n");
	*/

	elektraPluginSetData(handle, data);

	return 0;
}

int elektraJniClose(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	Data *data = elektraPluginGetData(handle);

	(*data->jvm)->DestroyJavaVM(data->jvm);
	free(data);

	return 1; /* success */
}

int elektraJniGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraJniSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1; /* success */
}

int elektraJniError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1; /* success */
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

