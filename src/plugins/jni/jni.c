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
	JavaVMInitArgs vm_args;
	jclass cls;
	jmethodID mid;
	jstring jstr;
	jobjectArray args;
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

	JavaVMOption* options = malloc(sizeof(JavaVMOption[1]));
	options[0].optionString = "-Djava.class.path=/usr/lib/java:/home/markus/Projekte/Elektra/libelektra/src/bindings/jna";
	data->vm_args.version = JNI_VERSION_1_8;
	data->vm_args.nOptions = 1;
	data->vm_args.options = options;
	data->vm_args.ignoreUnrecognized = 0;

	jint res = JNI_CreateJavaVM(&data->jvm,
			(void**)&data->env,
			(void**)&data->vm_args);
	if (res < 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot create Java VM");
		return -1;
	}
	free(options);

	data->cls = (*data->env)->FindClass(data->env, "PluginDemo");
	if (data->cls == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find DemoPlugin");
		return -1;
	}

	data->mid = (*data->env)->GetStaticMethodID(data->env,
			data->cls, "main", "([Ljava/lang/String;)V");
	if (data->mid == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Cannot find main");
		return -1;
	}

	data->jstr = (*data->env)->NewStringUTF(data->env, " from C!");
	if (data->jstr == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Out of memory, String");
		return -1;
	}

	data->args = (*data->env)->NewObjectArray(data->env, 1, 
		(*data->env)->FindClass(data->env, "java/lang/String"),
		data->jstr);
	if (data->args == 0)
	{
		ELEKTRA_SET_ERROR(26, errorKey, "Out of memory, ObjectArray");
		return -1;
	}

	(*data->env)->CallStaticVoidMethod(data->env,
		data->cls, data->mid, data->args);

	elektraPluginSetData(handle, data);

	return 1; /* success */
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

