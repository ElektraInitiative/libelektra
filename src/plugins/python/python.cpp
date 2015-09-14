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

#include "python.hpp"
#include <key.hpp>
#include <keyset.hpp>
#include <Python.h>
#include <libgen.h>
#include <pthread.h>
#include <iostream>
#include "runtime.h"

using namespace ckdb;

#define MODULE_NAME "python"

static PyObject *Python_fromSWIG(ckdb::Key *key)
{
	swig_type_info *ti = SWIG_TypeQuery("kdb::Key *");
	if (key == NULL || ti == NULL)
		return Py_None;
	return SWIG_NewPointerObj(new kdb::Key(key), ti, 0);
}

static PyObject *Python_fromSWIG(ckdb::KeySet *keyset)
{
	swig_type_info *ti = SWIG_TypeQuery("kdb::KeySet *");
	if (keyset == NULL || ti == NULL)
		return Py_None;
	return SWIG_NewPointerObj(new kdb::KeySet(keyset), ti, 0);
}

extern "C"
{

typedef struct
{
	PyObject *kdbModule;
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
		std::cerr << "Error while calling python function" << std::endl;
		PyErr_Print();
	}
	else
	{
		if (!PyLong_Check(res))
			std::cerr << "Error: return value is no integer" << std::endl;
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

	std::ostringstream appendcmd;
	appendcmd << "import sys\n" \
	"if not '" << path << "' in sys.path:\n" \
	"	sys.path.append ('" << path << "')\n";

	PyGILState_STATE gstate = PyGILState_Ensure();
	int ret = (PyRun_SimpleString(appendcmd.str().c_str()) == 0);
	PyGILState_Release(gstate);
	return ret;
}

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned open_cnt = 0;

int elektraPythonOpen(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	printf("[ELEKTRA] open -->\n");
	KeySet *config = elektraPluginGetConfig(handle);
	Key *script = ksLookupByName(config, "/script", 0);
	/* success if no script to execute */
	if (script == NULL || keyString(script) == NULL)
		return 0;
	printf("open handle=%p\n", handle);

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
		std::cerr << "ERROR: Unable to extend sys.path" << std::endl;
		return -1;
	}
	free(tmpScript);

	/* import kdb */
	PyObject *kdbModule = Python_ImportModule("kdb");
	if (kdbModule == NULL)
	{
		std::cerr << "ERROR: Failed to import kdb module" << std::endl;
		PyErr_Print();
		return -1;
	}

	/* import module/script */
	tmpScript = strdup(keyString(script));
	char *bname = basename(tmpScript);
	size_t bname_len = strlen(bname);
	if (bname_len >= 4 && strcmp(bname + bname_len - 3, ".py") == 0)
		bname[bname_len - 3] = '\0';

	PyObject *pModule = Python_ImportModule(bname);
	if (pModule == NULL)
	{
		std::cerr << "ERROR: Failed to import python module" << std::endl;
		PyErr_Print();
		return -1;
	}
	free(tmpScript);

	/* store modules */
	moduleData *data = new moduleData;
	data->kdbModule = kdbModule;
	data->pModule = pModule;
	elektraPluginSetData(handle, data);

	/* call python function */
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *func = PyObject_GetAttrString(pModule, "elektraOpen");
	if (func)
	{
		PyObject *pyErrorKey = Python_fromSWIG(errorKey);
		PyObject *args = Py_BuildValue("(O)", pyErrorKey);
		ret = Python_CallFunction_Int(func, args);
		Py_DECREF(pyErrorKey);
		Py_DECREF(args);
		Py_DECREF(func);
	}
	PyGILState_Release(gstate);

	return ret;
}

int elektraPythonClose(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	printf("[ELEKTRA] <-- close\n");
	moduleData *data = static_cast<moduleData *>(elektraPluginGetData(handle));
	if (data == NULL)
		return 0;
	printf("close handle=%p\n", handle);

	/* call python function */
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	if (data->pModule != NULL)
	{
		PyObject *func = PyObject_GetAttrString(data->pModule, "elektraClose");
		if (func)
		{
			PyObject *pyErrorKey = Python_fromSWIG(errorKey);
			PyObject *args = Py_BuildValue("(O)", pyErrorKey);
			ret = Python_CallFunction_Int(func, args);
			Py_DECREF(pyErrorKey);
			Py_DECREF(args);
			Py_DECREF(func);
		}

		Py_DECREF(data->pModule);
	}

	Py_XDECREF(data->kdbModule);

	PyGILState_Release(gstate);

	/* destroy python if plugin isn't used anymore */
	printf("destroying python\n");
	pthread_mutex_lock(&mutex);
	open_cnt--;
	if (!open_cnt && Py_IsInitialized())
		Py_Finalize();
	pthread_mutex_unlock(&mutex);

	delete data;
	return ret;
}

int elektraPythonGet(ckdb::Plugin *handle ELEKTRA_UNUSED, ckdb::KeySet *returned,
	ckdb::Key *parentKey)
{
#define _MODULE_CONFIG_PATH "system/elektra/modules/" MODULE_NAME

	printf("XXX get %s\n", keyName(parentKey));
	printf("get handle=%p\n", handle);

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

int elektraPythonSet(ckdb::Plugin *handle ELEKTRA_UNUSED,
	ckdb::KeySet *returned ELEKTRA_UNUSED, ckdb::Key *parentKey ELEKTRA_UNUSED)
{
	printf("XXX set\n");
	return 1;
}

int elektraPythonError(ckdb::Plugin *handle ELEKTRA_UNUSED,
	ckdb::KeySet *returned ELEKTRA_UNUSED, ckdb::Key *parentKey ELEKTRA_UNUSED)
{
	printf("XXX error\n");
	return 0;
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(python)
{
	return elektraPluginExport(MODULE_NAME,
		ELEKTRA_PLUGIN_OPEN,  &elektraPythonOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraPythonClose,
		ELEKTRA_PLUGIN_GET,   &elektraPythonGet,
		ELEKTRA_PLUGIN_SET,   &elektraPythonSet,
		ELEKTRA_PLUGIN_ERROR, &elektraPythonError,
		ELEKTRA_PLUGIN_END);
}

}
