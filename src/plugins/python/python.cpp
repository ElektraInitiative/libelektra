/**
 * \file
 *
 * \brief Plugin which acts as proxy and calls other plugins written in python
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef SWIG_TYPE_TABLE
# error Build system error, SWIG_TYPE_TABLE is not defined
#endif

#include <Python.h>

#ifndef HAVE_KDBCONFIG
# include <kdbconfig.h>
#endif
#include <kdbhelper.h>
#include SWIG_RUNTIME
#include "python.hpp"

#include <key.hpp>
#include <keyset.hpp>
#include <libgen.h>
#include <pthread.h>
#include <iostream>

using namespace ckdb;
#include <kdberrors.h>

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
	PyObject *instance;
	int printError;
	int shutdown;
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

static int Python_CallFunction_Int(moduleData *data, PyObject *object,
		PyObject *args, ckdb::Key *errorKey)
{
	int ret = -1;
	PyGILState_STATE gstate = PyGILState_Ensure();

	PyObject *res = Python_CallFunction(object, args);
	if (!res)
	{
		ELEKTRA_SET_ERROR(111, errorKey,
				"Error while calling python function");
		if (data->printError)
			PyErr_Print();
	}
	else
	{
#if PY_MAJOR_VERSION >= 3
		if (!PyLong_Check(res))
#else
		if (!PyInt_Check(res))
#endif
			ELEKTRA_SET_ERROR(111, errorKey,
					"Error: return value is no integer");
		else
			ret = PyLong_AsLong(res);
	}

	PyGILState_Release(gstate);
	Py_XDECREF(res);
	return ret;
}

static int Python_CallFunction_Helper1(moduleData *data, const char *funcName,
	ckdb::Key *errorKey)
{
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *func = PyObject_GetAttrString(data->instance, funcName);
	if (func)
	{
		PyObject *arg0 = Python_fromSWIG(errorKey);
		PyObject *args = Py_BuildValue("(O)", arg0);
		ret = Python_CallFunction_Int(data, func, args, errorKey);
		Py_DECREF(arg0);
		Py_DECREF(args);
		Py_DECREF(func);
	}
	PyGILState_Release(gstate);
	return ret;
}

static int Python_CallFunction_Helper2(moduleData *data, const char *funcName,
	ckdb::KeySet *returned, ckdb::Key *parentKey)
{
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *func = PyObject_GetAttrString(data->instance, funcName);
	if (func)
	{
		PyObject *arg0 = Python_fromSWIG(returned);
		PyObject *arg1 = Python_fromSWIG(parentKey);
		PyObject *args = Py_BuildValue("(OO)", arg0, arg1);
		ret = Python_CallFunction_Int(data, func, args, parentKey);
		Py_DECREF(arg0);
		Py_DECREF(arg1);
		Py_DECREF(args);
		Py_DECREF(func);
	}
	PyGILState_Release(gstate);
	return ret;
}

static int Python_AppendToSysPath(const char *path)
{
	if (path == NULL)
		return 0;

	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *sysPath = PySys_GetObject((char *)"path");
	PyObject *pyPath  = PyUnicode_FromString(path);
	PyList_Append(sysPath, pyPath);
	Py_DECREF(pyPath);
	PyGILState_Release(gstate);
	return 1;
}

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned open_cnt = 0;

int ELEKTRA_PLUGIN_FUNCTION(Python, Open)(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	/* store modules */
	moduleData *data = new moduleData;
	data->instance   = NULL;
	data->printError = 0;
	data->shutdown   = 0;
	elektraPluginSetData(handle, data);

	KeySet *config = elektraPluginGetConfig(handle);
	data->printError = (ksLookupByName(config, "/print", 0) != NULL);
	data->shutdown   = (!!strcmp(keyString(ksLookupByName(config, "/shutdown", 0)), "0"));

	Key *script = ksLookupByName(config, "/script", 0);
	if (script == NULL)
		return 0; // success if no script to execute
	if (keyString(script) == NULL)
	{
		ELEKTRA_SET_ERROR(111, errorKey, "No python script set");
		return -1;
	}

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

	/* import kdb */
	PyObject *kdbModule = Python_ImportModule("kdb");
	if (kdbModule == NULL)
	{
		ELEKTRA_SET_ERROR(111, errorKey, "Unable to import kdb module");
		if (data->printError)
			PyErr_Print();
		return -1;
	}
	Py_XDECREF(kdbModule);

	/* extend sys path */
	char *tmpScript = elektraStrDup(keyString(script));
	const char *dname = dirname(tmpScript);
	if (!Python_AppendToSysPath(dname))
	{
		ELEKTRA_SET_ERROR(111, errorKey, "Unable to extend sys.path");
		elektraFree(tmpScript);
		return -1;
	}
	elektraFree(tmpScript);

	/* import module/script */
	tmpScript = elektraStrDup(keyString(script));
	char *bname = basename(tmpScript);
	size_t bname_len = strlen(bname);
	if (bname_len >= 4 && strcmp(bname + bname_len - 3, ".py") == 0)
		bname[bname_len - 3] = '\0';

	PyObject *pModule = Python_ImportModule(bname);
	if (pModule == NULL)
	{
		ELEKTRA_SET_ERRORF(111, errorKey,"Unable to import python script %s",
				keyString(script));
		if (data->printError)
			PyErr_Print();
		elektraFree(tmpScript);
		return -1;
	}
	elektraFree(tmpScript);

	/* get class */
	PyGILState_STATE gstate = PyGILState_Ensure();
	PyObject *klass = PyObject_GetAttrString(pModule, "ElektraPlugin");
	Py_DECREF(pModule);
	PyGILState_Release(gstate);
	if (klass == NULL)
	{
		ELEKTRA_SET_ERROR(111, errorKey,
				"Module doesn't provide a ElektraPlugin class");
		if (data->printError)
			PyErr_Print();
		return -1;
	}

	/* create instance of class */
	gstate = PyGILState_Ensure();
	PyObject *inst_args = Py_BuildValue("()");
	PyObject *inst = PyEval_CallObject(klass, inst_args);
	Py_DECREF(klass);
	Py_DECREF(inst_args);
	PyGILState_Release(gstate);
	if (inst == NULL)
	{
		ELEKTRA_SET_ERROR(111, errorKey,
				"Unable to create instance of ElektraPlugin");
		if (data->printError)
			PyErr_Print();
		return -1;
	}
	data->instance = inst;

	/* call python function */
	return Python_CallFunction_Helper1(data, "open", errorKey);
}

int ELEKTRA_PLUGIN_FUNCTION(Python, Close)(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	moduleData *data = static_cast<moduleData *>(elektraPluginGetData(handle));
	if (data == NULL)
		return 0;

	/* call python function */
	int ret = 0;
	PyGILState_STATE gstate = PyGILState_Ensure();
	if (data->instance != NULL)
	{
		ret = Python_CallFunction_Helper1(data, "close", errorKey);

		/* clean up references */
		Py_DECREF(data->instance);
		data->instance = NULL;
	}
	PyGILState_Release(gstate);

	/* destroy python if plugin isn't used anymore */
	//FIXME python reinitialization is known to be buggy
	pthread_mutex_lock(&mutex);
	open_cnt--;
	if (!open_cnt && data->shutdown && Py_IsInitialized())
		Py_Finalize();
	pthread_mutex_unlock(&mutex);

	delete data;
	return ret;
}

int ELEKTRA_PLUGIN_FUNCTION(Python, Get)(ckdb::Plugin *handle, ckdb::KeySet *returned,
	ckdb::Key *parentKey)
{
	moduleData *data = static_cast<moduleData *>(elektraPluginGetData(handle));
	if (data != NULL)
		return Python_CallFunction_Helper2(data, "get", returned,
				parentKey);

#define _MODULE_CONFIG_PATH "system/elektra/modules/" ELEKTRA_PLUGIN_NAME
	if (!strcmp(keyName(parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet *n;
		ksAppend(returned, n = ksNew(30,
			keyNew(_MODULE_CONFIG_PATH,
				KEY_VALUE, "python interpreter waits for your orders", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports", KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/get",
				KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(Python, Get),
				KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/set",
				KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(Python, Set),
				KEY_END),
			keyNew(_MODULE_CONFIG_PATH "/exports/error",
				KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(Python, Error),
				KEY_END),
#include "readme_python.c"
			keyNew(_MODULE_CONFIG_PATH "/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel(n);
		return 1;
	}
	return 1;
}

int ELEKTRA_PLUGIN_FUNCTION(Python, Set)(ckdb::Plugin *handle, ckdb::KeySet *returned,
	ckdb::Key *parentKey)
{
	int ret = 0;
	moduleData *data = static_cast<moduleData *>(elektraPluginGetData(handle));
	if (data != NULL)
		ret = Python_CallFunction_Helper2(data, "set", returned,
				parentKey);
	return ret;
}

int ELEKTRA_PLUGIN_FUNCTION(Python, Error)(ckdb::Plugin *handle, ckdb::KeySet *returned,
	ckdb::Key *parentKey)
{
	int ret = 0;
	moduleData *data = static_cast<moduleData *>(elektraPluginGetData(handle));
	if (data != NULL)
		ret = Python_CallFunction_Helper2(data, "error", returned,
				parentKey);
	return ret;
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(python)
{
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,  &ELEKTRA_PLUGIN_FUNCTION(Python, Open),
		ELEKTRA_PLUGIN_CLOSE, &ELEKTRA_PLUGIN_FUNCTION(Python, Close),
		ELEKTRA_PLUGIN_GET,   &ELEKTRA_PLUGIN_FUNCTION(Python, Get),
		ELEKTRA_PLUGIN_SET,   &ELEKTRA_PLUGIN_FUNCTION(Python, Set),
		ELEKTRA_PLUGIN_ERROR, &ELEKTRA_PLUGIN_FUNCTION(Python, Error),
		ELEKTRA_PLUGIN_END);
}
}
