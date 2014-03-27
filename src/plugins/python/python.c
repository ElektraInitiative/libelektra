/**
 * \file
 *
 * \brief TODO
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "python.h"
#include <libgen.h>
#include <pthread.h>
#include <Python.h>

#define MODULE_NAME "python"

typedef struct
{
	PyObject *pModule;
} moduleData;

/* pythons repr() - a little debug helper - misses all Py_DECREF calls! */
#define Python_Repr(obj) \
	PyBytes_AS_STRING( \
		PyUnicode_AsEncodedString(PyObject_Repr(obj), "utf-8", "Error ~") \
	)

static PyObject *Python_ImportModule(const char *name)
{
	if (!name)
		return NULL;

    PyGILState_STATE gstate = PyGILState_Ensure();
    PyObject *module = PyImport_ImportModule(name);
    PyGILState_Release(gstate);
    return module;
}

static PyObject *Python_CallFunction(PyObject *object, PyObject *args)
{
	PyGILState_STATE gstate = PyGILState_Ensure();

	if (!PyCallable_Check(object))
	{
		PyGILState_Release(gstate);
		return NULL;
	}

	PyObject *res = PyObject_CallObject(object, args ? args : PyTuple_New (0));
	PyGILState_Release(gstate);
	Py_XINCREF(res);
	return res;
}

static int Python_CallFunction_Int(PyObject *object, PyObject *args)
{
	int ret = -1;
	PyGILState_STATE gstate = PyGILState_Ensure();

	PyObject *res = Python_CallFunction(object, args);
	if (!res)
	{
		printf("Error while calling python function\n");
		PyErr_Print();
	}
	else
	{
		if (!PyLong_Check(res))
			printf("Error: return value is no integer\n");
		else
			ret = PyLong_AsLong(res);
	}

	PyGILState_Release(gstate);
	Py_XDECREF(res);
	return ret;
}

static int Python_AppendToSysPath(const char *path)
{
	if (path == NULL)
		return 0;

#define _APPENDTOSYS_STR \
		"import sys\n" \
		"if not '%s' in sys.path:\n" \
		"	sys.path.append ('%s')\n"

	char *buf = malloc(sizeof(_APPENDTOSYS_STR) + 2*strlen(path) - 2*2 + 1);
	sprintf(buf, _APPENDTOSYS_STR, path, path);

	PyGILState_STATE gstate = PyGILState_Ensure();

	int ret = (PyRun_SimpleString(buf) == 0);

	PyGILState_Release(gstate);
	free(buf);
	return ret;
}

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned open_cnt = 0;

int elektraPythonOpen(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	printf("[ELEKTRA] open -->\n");
	KeySet *config = elektraPluginGetConfig(handle);
	Key *script = ksLookupByName(config, "/script", 0);
	/* success if no script to execute */
	if (script == NULL || keyString(script) == NULL)
		return 0;

	/* initialize python interpreter - only once */
	pthread_mutex_lock(&mutex);
	if (!Py_IsInitialized())
	{
		Py_Initialize();
		PyEval_InitThreads();
		if (!Py_IsInitialized())
		{
			pthread_mutex_unlock(&mutex);
			return -1;
		}
	}
	open_cnt++;
	pthread_mutex_unlock(&mutex);

	/* extend sys path */
	char *tmpScript = strdup(keyString(script));
	const char *dname = dirname(tmpScript);
	if (!Python_AppendToSysPath(dname))
	{
		printf("ERROR: Unable to extend sys.path");
		return -1;
	}
	free(tmpScript);

	/* import module/script */
	tmpScript = strdup(keyString(script));
	char *bname = basename(tmpScript);
	size_t bname_len = strlen(bname);
	if (bname_len >= 4 && strcmp(bname + bname_len - 3, ".py") == 0)
		bname[bname_len - 3] = '\0';

	PyObject *pModule = Python_ImportModule(bname);
	if (pModule == NULL)
	{
		printf("ERROR: Failed to import module\n");
		PyErr_Print();
		return -1;
	}
	free(tmpScript);

	/* store module */
	moduleData *data = malloc(sizeof(moduleData));
	data->pModule = pModule;
	elektraPluginSetData(handle, data);

	/* call python function */
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *func = PyObject_GetAttrString(pModule, "elektraOpen");
	if (func)
	{
		ret = Python_CallFunction_Int(func, NULL);
		Py_DECREF(func);
	}
	PyGILState_Release(gstate);

	return ret;
}

int elektraPythonClose(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	printf("[ELEKTRA] <-- close\n");
	moduleData *data = elektraPluginGetData(handle);
	if (data == NULL)
		return 0;

	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	if (data->pModule != NULL)
	{
		PyObject *func = PyObject_GetAttrString(data->pModule, "elektraClose");
		if (func)
		{
			ret = Python_CallFunction_Int(func, NULL);
			Py_DECREF(func);
		}

		Py_XDECREF(data->pModule);
	}
	PyGILState_Release(gstate);

	/* destroy python if plugin isn't used anymore */
	pthread_mutex_lock(&mutex);
	open_cnt--;
	if (!open_cnt && Py_IsInitialized())
		Py_Finalize();
	pthread_mutex_unlock(&mutex);

	free(data);
	return ret;
}

int elektraPythonGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
#define _MODULE_CONFIG_PATH "system/elektra/modules/" MODULE_NAME

	printf("XXX get %s\n", keyName(parentKey));

	KeySet *config = elektraPluginGetConfig(handle);
	Key *k = ksLookupByName(config, "/path", 0);
	printf("%p\n", k);
	if (!strcmp(keyName(parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet *n;
		ksAppend(returned, n = ksNew(30,
			keyNew(_MODULE_CONFIG_PATH,
				KEY_VALUE, "python interpreter waits for your orders", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/get",
				KEY_FUNC, elektraPythonGet,
				KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/set",
				KEY_FUNC, elektraPythonSet,
				KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/error",
				KEY_FUNC, elektraPythonError,
				KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/author",
				KEY_VALUE, "Manuel Mausz <manuel-elektra@mausz.at>", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/description",
				KEY_VALUE, "TODO", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/provides",
				KEY_VALUE, "filter", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/placements",
				KEY_VALUE, "prerollback postrollback pregetstorage postgetstorage presetstorage precommit postcommit", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew("system/elektra/modules/FOO",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel(n);
		return 1;
	}
	return 1;
}

int elektraPythonSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	printf("XXX set\n");
	return 1;
}

int elektraPythonError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED,
	Key *parentKey ELEKTRA_UNUSED)
{
	printf("XXX error\n");
	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(python)
{
	printf("loaded\n");
	return elektraPluginExport(MODULE_NAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraPythonOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraPythonClose,
		ELEKTRA_PLUGIN_GET,	&elektraPythonGet,
		ELEKTRA_PLUGIN_SET,	&elektraPythonSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraPythonError,
		ELEKTRA_PLUGIN_END);
}
