/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <string.h>

#include <jni.h>
#include <stdlib.h>


#include <kdberrors.h>
#include <kdbplugin.h>

// forward declarations
int elektraJniOpen (Plugin * handle, Key * errorKey);
int elektraJniClose (Plugin * handle, Key * errorKey);
int elektraJniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraJniSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraJniError (Plugin * handle, KeySet * ks, Key * parentKey);

// plugin's data handle
typedef struct
{
	JNIEnv * env;
	JavaVM * jvm;
	jclass clsPlugin;
	jclass clsKey;
	jclass clsKeySet;
	int module;
	int printException;
	jmethodID midKeyConstr;
	jmethodID midKeySetConstr;
	jmethodID midKeyRelease;
	jmethodID midKeySetRelease;
	jobject plugin;
} Data;

static void checkException (Data * data, const char * when, Key * warningKey)
{
	if ((*data->env)->ExceptionCheck (data->env))
	{
		if (data->printException)
		{
			(*data->env)->ExceptionDescribe (data->env);
		}
		jthrowable ex = (*data->env)->ExceptionOccurred (data->env);
		jmethodID toString = (*data->env)
					     ->GetMethodID (data->env, (*data->env)->FindClass (data->env, "java/lang/Object"), "toString",
							    "()Ljava/lang/String;");
		jstring estr = (jstring) (*data->env)->CallObjectMethod (data->env, ex, toString);

		jboolean iseCopy = JNI_FALSE;
		const char * which = "unknown";
		if (estr)
		{
			which = (*data->env)->GetStringUTFChars (data->env, estr, &iseCopy);
		}

		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (warningKey, "During \"%s\", java exception was thrown: %s", when, which);

		if (iseCopy == JNI_TRUE)
		{
			(*data->env)->ReleaseStringUTFChars (data->env, estr, which);
		}
		(*data->env)->ExceptionClear (data->env);
	}
}

static int call1Arg (Data * data, Key * errorKey, const char * method)
{
	jobject jerrorKey = (*data->env)->NewObject (data->env, data->clsKey, data->midKeyConstr, errorKey);
	checkException (data, method, errorKey);
	if (jerrorKey == 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Cannot create errorKey in %s", method);
		return -1;
	}

	jmethodID mid = (*data->env)->GetMethodID (data->env, data->clsPlugin, method, "(Lorg/libelektra/Key;)I");
	checkException (data, method, errorKey);
	if (mid == 0)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Cannot find org/libelektra/Key in %s", method);
		return -1;
	}

	jint result = 0;
	result = (*data->env)->CallIntMethod (data->env, data->plugin, mid, jerrorKey);
	if ((*data->env)->ExceptionCheck (data->env))
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "Method '%s' failed with exception", method);
		result = -1;
	}
	checkException (data, method, errorKey);

	(*data->env)->CallVoidMethod (data->env, jerrorKey, data->midKeyRelease);
	checkException (data, method, errorKey);

	return result;
}

static int call2Arg (Data * data, KeySet * ks, Key * errorKey, const char * method)
{
	jobject jks = (*data->env)->NewObject (data->env, data->clsKeySet, data->midKeySetConstr, ks);
	checkException (data, method, errorKey);
	if (jks == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot create ks");
		return -1;
	}

	jobject jkey = (*data->env)->NewObject (data->env, data->clsKey, data->midKeyConstr, errorKey);
	checkException (data, method, errorKey);
	if (jkey == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot create key");
		return -1;
	}

	jmethodID mid = (*data->env)->GetMethodID (data->env, data->clsPlugin, method, "(Lorg/libelektra/KeySet;Lorg/libelektra/Key;)I");
	checkException (data, method, errorKey);
	if (mid == 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Cannot find %s", method);
		return -1;
	}

	jint result = 0;
	result = (*data->env)->CallIntMethod (data->env, data->plugin, mid, jks, jkey);
	if ((*data->env)->ExceptionCheck (data->env))
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "Method '%s' failed with exception", method);
		result = -1;
	}
	checkException (data, method, errorKey);

	(*data->env)->CallVoidMethod (data->env, jks, data->midKeySetRelease);
	checkException (data, method, errorKey);

	(*data->env)->CallVoidMethod (data->env, jkey, data->midKeyRelease);
	checkException (data, method, errorKey);

	return result;
}

int elektraJniOpen (Plugin * handle, Key * errorKey)
{
	Data * data = elektraMalloc (sizeof (Data));
	data->module = 0;
	data->printException = 0;
	elektraPluginSetData (handle, data);

	KeySet * config = elektraPluginGetConfig (handle);
	Key * k = ksLookupByName (config, "/module", 0);
	if (k)
	{
		data->module = 1;
		return 0;
	}

	k = ksLookupByName (config, "/print", 0);
	if (k)
	{
		data->printException = 1;
	}

	k = ksLookupByName (config, "/classpath", 0);
	if (!k)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not find plugin config /classpath");
		return -1;
	}
	char classpatharg[] = "-Djava.class.path=";
	char * classpath = elektraMalloc (sizeof (classpatharg) + keyGetValueSize (k));
	strcpy (classpath, classpatharg);
	strcat (classpath, keyString (k));

	k = ksLookupByName (config, "/option", 0);
	char * option = 0;
	if (!k)
	{
		option = "-verbose:gc,class,jni";
	}
	else
	{
		option = (char *) keyString (k);
	}

	k = ksLookupByName (config, "/ignore", 0);
	jboolean ign = JNI_FALSE;
	if (k) ign = JNI_TRUE;

	/* TODO: check if JVM is already started:
	jsize nVMs;
	JNI_GetCreatedJavaVMs(NULL, 0, &nVMs); // get array length
	JavaVM** buffer = elektraMalloc(nVMs, sizeof(JavaVM*));
	JNI_GetCreatedJavaVMs(buffer, nVMs, &nVMs); // get data
	*/

	JavaVMInitArgs vmArgs;
	JavaVMOption options[2];
	options[0].optionString = classpath;
	options[1].optionString = option;
	vmArgs.version = JNI_VERSION_1_8;
	vmArgs.nOptions = 2;
	vmArgs.options = options;
	vmArgs.ignoreUnrecognized = ign;

	jint res = JNI_CreateJavaVM (&data->jvm, (void **) &data->env, (void **) &vmArgs);
	elektraFree (classpath);
	if (res < 0)
	{
		switch (res)
		{
		case JNI_EDETACHED:
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Cannot create Java VM: Thread detached from the VM");
			return -1;
		case JNI_EVERSION:
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Cannot create Java VM: JNI version error");
			return -1;
		case JNI_ENOMEM:
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
			return -1;
		case JNI_EEXIST:
			ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot create Java VM: VM already created");
			return -1;
		case JNI_EINVAL:
			ELEKTRA_SET_INTERFACE_ERROR (errorKey, "Cannot create Java VM: Invalid arguments");
			return -1;
		default:
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Cannot create Java VM: Unknown error");
			return -1;
		}
	}

	k = ksLookupByName (config, "/classname", 0);
	if (!k)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not find plugin config /classname");
		return -1;
	}

	const char * classname = keyString (k);

	data->clsPlugin = (*data->env)->FindClass (data->env, classname);
	if (data->clsPlugin == 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Cannot find class %s", classname);
		return -1;
	}

	data->clsKey = (*data->env)->FindClass (data->env, "org/libelektra/Key");
	if (data->clsKey == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find class Key");
		return -1;
	}

	data->clsKeySet = (*data->env)->FindClass (data->env, "org/libelektra/KeySet");
	if (data->clsKeySet == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find class KeySet");
		return -1;
	}

	data->midKeyConstr = (*data->env)->GetMethodID (data->env, data->clsKey, "<init>", "(J)V");
	if (data->midKeyConstr == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find constructor of Key");
		return -1;
	}

	data->midKeySetConstr = (*data->env)->GetMethodID (data->env, data->clsKeySet, "<init>", "(J)V");
	if (data->midKeySetConstr == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find constructor of KeySet");
		return -1;
	}

	data->midKeyRelease = (*data->env)->GetMethodID (data->env, data->clsKey, "release", "()V");
	if (data->midKeyRelease == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find release of Key");
		return -1;
	}

	data->midKeySetRelease = (*data->env)->GetMethodID (data->env, data->clsKeySet, "release", "()V");
	if (data->midKeySetRelease == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find release of KeySet");
		return -1;
	}

	jmethodID midPluginConstructor = (*data->env)->GetMethodID (data->env, data->clsPlugin, "<init>", "()V");
	if (midPluginConstructor == 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Cannot find constructor of plugin");
		return -1;
	}

	data->plugin = (*data->env)->NewObject (data->env, data->clsPlugin, midPluginConstructor);
	checkException (data, "creating plugin", errorKey);
	if (data->plugin == 0)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (errorKey, "Cannot create plugin");
		return -1;
	}

	return call2Arg (data, config, errorKey, "open");
}

int elektraJniClose (Plugin * handle, Key * errorKey)
{
	Data * data = elektraPluginGetData (handle);

	if (!data)
	{
		return 0;
	}

	if (data->module == 1)
	{
		elektraFree (data);
		return 0;
	}

	int ret = call1Arg (data, errorKey, "close");

	(*data->jvm)->DestroyJavaVM (data->jvm);
	elektraFree (data);

	return ret;
}

int elektraJniGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/jni"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/jni", KEY_VALUE, "jni plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/jni/exports", KEY_END),
			       keyNew ("system/elektra/modules/jni/exports/open", KEY_FUNC, elektraJniOpen, KEY_END),
			       keyNew ("system/elektra/modules/jni/exports/close", KEY_FUNC, elektraJniClose, KEY_END),
			       keyNew ("system/elektra/modules/jni/exports/get", KEY_FUNC, elektraJniGet, KEY_END),
			       keyNew ("system/elektra/modules/jni/exports/set", KEY_FUNC, elektraJniSet, KEY_END),
			       keyNew ("system/elektra/modules/jni/exports/error", KEY_FUNC, elektraJniError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/jni/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);
	}

	Data * data = elektraPluginGetData (handle);

	if (!data || data->module == 1)
	{
		return 0;
	}

	return call2Arg (data, returned, parentKey, "get");
}

int elektraJniSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Data * data = elektraPluginGetData (handle);
	return call2Arg (data, returned, parentKey, "set");
}

int elektraJniError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Data * data = elektraPluginGetData (handle);
	return call2Arg (data, returned, parentKey, "error");
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("jni",
		ELEKTRA_PLUGIN_OPEN,	&elektraJniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraJniClose,
		ELEKTRA_PLUGIN_GET,	&elektraJniGet,
		ELEKTRA_PLUGIN_SET,	&elektraJniSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraJniError,
		ELEKTRA_PLUGIN_END);
}

