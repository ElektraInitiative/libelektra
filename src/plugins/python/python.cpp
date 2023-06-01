/**
 * @file
 *
 * @brief Plugin which acts as proxy and calls other plugins written in python
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef SWIG_TYPE_TABLE
#error Build system error, SWIG_TYPE_TABLE is not defined
#endif

#include <Python.h>

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif
#include <internal/utility/alloc.h>
#include SWIG_RUNTIME
#include "./python.hpp"

#include <config.h>
#include <internal/pluginprocess.h>
#include <key.hpp>
#include <keyset.hpp>
#include <libgen.h>
#include <mutex>

#include <elektra/core/errors.h>
using namespace ckdb;

#define PYTHON_PLUGIN_NAME_STR2(x) ELEKTRA_QUOTE (x)
#define PYTHON_PLUGIN_NAME_STR PYTHON_PLUGIN_NAME_STR2 (PYTHON_PLUGIN_NAME)

static PyObject * Python_fromSWIG (ckdb::Key * key)
{
	swig_type_info * ti = SWIG_TypeQuery ("kdb::Key *");
	if (key == nullptr || ti == nullptr) return Py_None;
	return SWIG_NewPointerObj (new kdb::Key (key), ti, 0);
}

static PyObject * Python_fromSWIG (ckdb::KeySet * keyset)
{
	swig_type_info * ti = SWIG_TypeQuery ("kdb::KeySet *");
	if (keyset == nullptr || ti == nullptr) return Py_None;
	return SWIG_NewPointerObj (new kdb::KeySet (keyset), ti, 0);
}

class Python_LockSwap
{
public:
	Python_LockSwap (PyThreadState * newstate)
	{
		gstate = PyGILState_Ensure ();
		tstate = PyThreadState_Swap (newstate);
	}

	~Python_LockSwap ()
	{
		PyThreadState_Swap (tstate);
		PyGILState_Release (gstate);
	}

private:
	PyGILState_STATE gstate;
	PyThreadState * tstate;
};

typedef struct
{
	PyThreadState * tstate;
	PyObject * instance;
	Key * script;
	int printError;
} moduleData;

static int Python_AppendToSysPath (const char * path)
{
	if (path == nullptr) return 0;

	PyObject * sysPath = PySys_GetObject ((char *) "path");
	PyObject * pyPath = PyUnicode_FromString (path);

	if (PyList_Append (sysPath, pyPath) == -1)
	{
		Py_DECREF (pyPath);
		return 0;
	}

	Py_DECREF (pyPath);
	return 1;
}

static PyObject * Python_CallFunction (PyObject * object, PyObject * args)
{
	if (!PyCallable_Check (object)) return nullptr;

	PyObject * res = PyObject_CallObject (object, args ? args : PyTuple_New (0));
	Py_XINCREF (res);
	return res;
}

static int Python_CallFunction_Int (moduleData * data, PyObject * object, PyObject * args, ckdb::Key * errorKey)
{
	int ret = -1;
	PyObject * res = Python_CallFunction (object, args);
	if (!res)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (errorKey, "Error while calling python function of script %s%s",
						       keyString (data->script),
						       data->printError ? "" : ", use /print to print error messages");
		if (data->printError) PyErr_Print ();
	}
	else
	{
#if PY_MAJOR_VERSION >= 3
		if (!PyLong_Check (res))
#else
		if (!PyInt_Check (res))
#endif
			ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (errorKey, "Python return value is no integer");
		else
			ret = PyLong_AsLong (res);
	}

	Py_XDECREF (res);
	return ret;
}

static int Python_CallFunction_Helper1 (moduleData * data, const char * funcName, ckdb::Key * errorKey)
{
	int ret = 0;
	Python_LockSwap pylock (data->tstate);
	PyObject * func = PyObject_GetAttrString (data->instance, funcName);
	if (func)
	{
		PyObject * arg0 = Python_fromSWIG (errorKey);
		PyObject * args = Py_BuildValue ("(O)", arg0);
		ret = Python_CallFunction_Int (data, func, args, errorKey);
		Py_DECREF (arg0);
		Py_DECREF (args);
		Py_DECREF (func);
	}
	return ret;
}

static int Python_CallFunction_Helper2 (moduleData * data, const char * funcName, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	int ret = 0;
	Python_LockSwap pylock (data->tstate);
	PyObject * func = PyObject_GetAttrString (data->instance, funcName);
	if (func)
	{
		PyObject * arg0 = Python_fromSWIG (returned);
		PyObject * arg1 = Python_fromSWIG (parentKey);
		PyObject * args = Py_BuildValue ("(OO)", arg0, arg1);
		ret = Python_CallFunction_Int (data, func, args, parentKey);
		Py_DECREF (arg0);
		Py_DECREF (arg1);
		Py_DECREF (args);
		Py_DECREF (func);
	}
	return ret;
}

static void Python_Shutdown (moduleData * data)
{
	/* destroy python if plugin isn't used anymore */
	if (Py_IsInitialized ())
	{
		// Do we have a sub interpreter?
		if (data->tstate)
		{
			Python_LockSwap pylock (data->tstate);

			/* clean up references */
			Py_XDECREF (data->instance);
			data->instance = nullptr;

			/* destroy sub interpreter */
			Py_EndInterpreter (data->tstate);
		}
		Py_Finalize ();
	}
}

static moduleData * createModuleData (ckdb::Plugin * handle)
{
	KeySet * config = elektraPluginGetConfig (handle);

	Key * script = ksLookupByName (config, "/script", 0);
	if (script == nullptr || !strlen (keyString (script)))
	{
		return 0;
	}

	/* create module data */
	auto data = new moduleData;
	data->tstate = nullptr;
	data->instance = nullptr;
	data->script = script;
	data->printError = (ksLookupByName (config, "/print", 0) != nullptr);
	return data;
}

extern "C" {
int PYTHON_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	ElektraPluginProcess * pp = static_cast<ElektraPluginProcess *> (elektraPluginGetData (handle));
	if (pp == nullptr)
	{
		moduleData * md = createModuleData (handle);
		if (!md)
		{
			if (ksLookupByName (elektraPluginGetConfig (handle), "/module", 0) != nullptr)
			{
				return ELEKTRA_PLUGIN_STATUS_SUCCESS; // by convention: success if /module exists
			}

			ELEKTRA_SET_INTERFACE_ERROR (errorKey, "No python script set, please pass a filename via /script");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if ((pp = elektraPluginProcessInit (errorKey)) == nullptr) return ELEKTRA_PLUGIN_STATUS_ERROR;

		elektraPluginProcessSetData (pp, md);
		elektraPluginSetData (handle, pp);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	moduleData * data = static_cast<moduleData *> (elektraPluginProcessGetData (pp));

	if (data->instance == nullptr)
	{
		/* initialize python interpreter if necessary */
		if (!Py_IsInitialized ())
		{
			Py_Initialize ();
			if (!Py_IsInitialized ())
			{
				goto error;
			}
		}
		/* init threads */
		PyEval_InitThreads ();

		/* acquire GIL */
		Python_LockSwap pylock (nullptr);

		/* create a new sub interpreter */
		data->tstate = Py_NewInterpreter ();
		if (data->tstate == nullptr)
		{
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Unable to create sub interpreter");
			goto error;
		}
		PyThreadState_Swap (data->tstate);

		/* extend sys path for kdb module */
		if (!Python_AppendToSysPath (ELEKTRA_PYTHON_SITE_PACKAGES))
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Unable to extend sys.path with built-in path '%s'",
							 ELEKTRA_PYTHON_SITE_PACKAGES);
			goto error;
		}

		/* extend sys path with user-defined path */
		const char * mname = keyString (ksLookupByName (elektraPluginGetConfig (handle), "/python/path", 0));
		if (!Python_AppendToSysPath (mname))
		{
			if (!mname)
			{
				mname = "<nullptr>";
			}
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Unable to extend sys.path with user-defined /python/path '%s'", mname);
			goto error;
		}

		/* import kdb */
		PyObject * kdbModule = PyImport_ImportModule ("kdb");
		if (kdbModule == nullptr)
		{
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Unable to import kdb module");
			goto error_print;
		}
		Py_XDECREF (kdbModule);

		/* extend sys path for standard plugins */
		if (!Python_AppendToSysPath (ELEKTRA_PYTHON_PLUGIN_FOLDER))
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Unable to extend sys.path with built-in plugin path '%s'",
							 ELEKTRA_PYTHON_PLUGIN_FOLDER);
			goto error;
		}

		/* extend sys path */
		char * tmpScript = elektraStrDup (keyString (data->script));
		const char * dname = dirname (tmpScript);
		if (!Python_AppendToSysPath (dname))
		{
			if (!dname)
			{
				dname = "<nullptr>";
			}
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Unable to extend sys.path with script dirname '%s'", dname);
			elektraFree (tmpScript);
			goto error;
		}
		elektraFree (tmpScript);

		/* import module/script */
		tmpScript = elektraStrDup (keyString (data->script));
		char * bname = basename (tmpScript);
		size_t bname_len = strlen (bname);
		if (bname_len >= 4 && strcmp (bname + bname_len - 3, ".py") == 0) bname[bname_len - 3] = '\0';

		PyObject * pModule = PyImport_ImportModule (bname);
		if (pModule == nullptr)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Unable to import python script '%s'", keyString (data->script));
			elektraFree (tmpScript);
			goto error_print;
		}
		elektraFree (tmpScript);

		/* get class */
		PyObject * klass = PyObject_GetAttrString (pModule, "ElektraPlugin");
		Py_DECREF (pModule);
		if (klass == nullptr)
		{
			ELEKTRA_SET_INTERFACE_ERROR (errorKey, "Module doesn't provide a ElektraPlugin class");
			goto error_print;
		}

		/* create instance of class */
		PyObject * inst_args = Py_BuildValue ("()");
		PyObject * inst = PyEval_CallObject (klass, inst_args);
		Py_DECREF (klass);
		Py_DECREF (inst_args);
		if (inst == nullptr)
		{
			ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (errorKey, "Unable to create instance of ElektraPlugin");
			goto error_print;
		}
		data->instance = inst;
	}

	/* call python function */
	return Python_CallFunction_Helper2 (data, "open", elektraPluginGetConfig (handle), errorKey);

error_print:
	if (data->printError) PyErr_Print ();
error:
	/* destroy python */
	Python_Shutdown (data);
	delete data;
	elektraPluginProcessSetData (pp, nullptr);
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

int PYTHON_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	ElektraPluginProcess * pp = static_cast<ElektraPluginProcess *> (elektraPluginGetData (handle));
	if (!pp) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	moduleData * data = static_cast<moduleData *> (elektraPluginProcessGetData (pp));
	if (elektraPluginProcessIsParent (pp))
	{
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp)
		{
			delete data;
			elektraPluginSetData (handle, NULL);
		}
		return result.result;
	}

	if (data == nullptr) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = Python_CallFunction_Helper1 (data, "close", errorKey);
	/* destroy python */
	Python_Shutdown (data);
	delete data;
	return ret;
}

int PYTHON_PLUGIN_FUNCTION (Get) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
#define _MODULE_CONFIG_PATH "system:/elektra/modules/" PYTHON_PLUGIN_NAME_STR
	if (!strcmp (keyName (parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew (_MODULE_CONFIG_PATH, KEY_VALUE, "python interpreter waits for your orders", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/get", KEY_FUNC, PYTHON_PLUGIN_FUNCTION (Get), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/set", KEY_FUNC, PYTHON_PLUGIN_FUNCTION (Set), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/error", KEY_FUNC, PYTHON_PLUGIN_FUNCTION (Error), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/open", KEY_FUNC, PYTHON_PLUGIN_FUNCTION (Open), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/close", KEY_FUNC, PYTHON_PLUGIN_FUNCTION (Close), KEY_END),
#include ELEKTRA_README
				     keyNew (_MODULE_CONFIG_PATH "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
	}

	ElektraPluginProcess * pp = static_cast<ElektraPluginProcess *> (elektraPluginGetData (handle));
	if (!pp) return 0;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	moduleData * data = static_cast<moduleData *> (elektraPluginProcessGetData (pp));
	if (data == nullptr) return 0;
	return Python_CallFunction_Helper2 (data, "get", returned, parentKey);
}

int PYTHON_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	ElektraPluginProcess * pp = static_cast<ElektraPluginProcess *> (elektraPluginGetData (handle));
	if (!pp) return 0;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	moduleData * data = static_cast<moduleData *> (elektraPluginProcessGetData (pp));
	if (data == nullptr) return 0;
	return Python_CallFunction_Helper2 (data, "set", returned, parentKey);
}

int PYTHON_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	ElektraPluginProcess * pp = static_cast<ElektraPluginProcess *> (elektraPluginGetData (handle));
	if (!pp) return 0;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	moduleData * data = static_cast<moduleData *> (elektraPluginProcessGetData (pp));
	if (data == nullptr) return 0;
	return Python_CallFunction_Helper2 (data, "error", returned, parentKey);
}

ckdb::Plugin * PYTHON_PLUGIN_EXPORT (PYTHON_PLUGIN_NAME)
{
	// clang-format off
	return elektraPluginExport(PYTHON_PLUGIN_NAME_STR,
		ELEKTRA_PLUGIN_OPEN,  &PYTHON_PLUGIN_FUNCTION(Open),
		ELEKTRA_PLUGIN_CLOSE, &PYTHON_PLUGIN_FUNCTION(Close),
		ELEKTRA_PLUGIN_GET,   &PYTHON_PLUGIN_FUNCTION(Get),
		ELEKTRA_PLUGIN_SET,   &PYTHON_PLUGIN_FUNCTION(Set),
		ELEKTRA_PLUGIN_ERROR, &PYTHON_PLUGIN_FUNCTION(Error),
		ELEKTRA_PLUGIN_END);
}
}
