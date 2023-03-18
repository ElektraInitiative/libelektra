/**
 * @file
 *
 * @brief Plugin which acts as proxy and calls other plugins written in lua
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef SWIG_TYPE_TABLE
#error Build system error, SWIG_TYPE_TABLE is not defined
#endif

#include "lua.hpp"
#include <internal/macros/utils.h>
#include <internal/utility/old_helper.h>

#include <key.hpp>
#include <keyset.hpp>
#include <libgen.h>

#include <lua.hpp>
extern "C" {
#include <lualib.h>
}
#include "runtime.h"

#include <elektra/kdb/errors.h>
using namespace ckdb;

#ifndef LUA_OK
#define LUA_OK 0
#endif

static void Lua_fromSWIG (lua_State * L, ckdb::Key * key)
{
	swig_type_info * ti = SWIG_TypeQuery (L, "kdb::Key *");
	if (key == NULL || ti == NULL)
		lua_pushnil (L);
	else
		SWIG_NewPointerObj (L, new kdb::Key (key), ti, 0);
}

static void Lua_fromSWIG (lua_State * L, ckdb::KeySet * keyset)
{
	swig_type_info * ti = SWIG_TypeQuery (L, "kdb::KeySet *");
	if (keyset == NULL || ti == NULL)
		lua_pushnil (L);
	else
		SWIG_NewPointerObj (L, new kdb::KeySet (keyset), ti, 0);
}

typedef struct
{
	lua_State * L;
	int printError;
	int shutdown;
} moduleData;

static void Lua_Shutdown (lua_State * L)
{
	if (L) lua_close (L);
}

static int Lua_Require (lua_State * L, const char * name)
{
	lua_getglobal (L, "require");
	lua_pushstring (L, name);
	int status = lua_pcall (L, 1, 1, 0);
	if (status == LUA_OK) lua_setglobal (L, name);
	return status;
}

static int Lua_CallFunction_Int (lua_State * L, int nargs, ckdb::Key * errorKey)
{
	int ret = -1;
	if (lua_pcall (L, nargs, 1, 0) != LUA_OK)
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, lua_tostring (L, -1));
	else
	{
		if (!lua_isnumber (L, -1))
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "Return value is no integer");
		else
			ret = lua_tonumber (L, -1);
	}
	return ret;
}

static int Lua_CallFunction_Helper1 (lua_State * L, const char * funcName, ckdb::Key * errorKey)
{
	int ret = 0;
	int top = lua_gettop (L);
	lua_getglobal (L, funcName);
	if (lua_isfunction (L, -1))
	{
		Lua_fromSWIG (L, errorKey);
		ret = Lua_CallFunction_Int (L, 1, errorKey);
	}
	lua_settop (L, top);
	return ret;
}

static int Lua_CallFunction_Helper2 (lua_State * L, const char * funcName, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	int ret = 0;
	int top = lua_gettop (L);
	lua_getglobal (L, funcName);
	if (lua_isfunction (L, -1))
	{
		Lua_fromSWIG (L, returned);
		Lua_fromSWIG (L, parentKey);
		ret = Lua_CallFunction_Int (L, 2, parentKey);
	}
	lua_settop (L, top);
	return ret;
}

extern "C" {
static void * Lua_alloc (void * ud ELEKTRA_UNUSED, void * ptr, size_t osize ELEKTRA_UNUSED, size_t nsize)
{
	if (nsize == 0)
	{
		elektraFree (ptr);
		return NULL;
	}
	return (elektraRealloc (&ptr, nsize) < 0) ? NULL : ptr;
}

int elektraLuaOpen (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	KeySet * config = elektraPluginGetConfig (handle);

	Key * script = ksLookupByName (config, "/script", 0);
	if (script == NULL || !strlen (keyString (script)))
	{
		if (ksLookupByName (config, "/module", 0) != NULL)
		{
			return 0; // by convention: success if /module exists
		}
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "No lua script set");
		return -1;
	}

	/* create module data */
	moduleData * data = new moduleData;

	/* init new lua state */
	if ((data->L = lua_newstate (Lua_alloc, NULL)) == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Unable to create new lua state");
		goto error;
	}

	/* load std. libs */
	luaL_openlibs (data->L);

	/* require kdb */
	if (Lua_Require (data->L, "kdb") != LUA_OK) goto error_print;

	/* load lua script */
	if (luaL_dofile (data->L, keyString (script))) goto error_print;

	/* store module data after everything is set up */
	elektraPluginSetData (handle, data);

	/* call lua function */
	return Lua_CallFunction_Helper2 (data->L, "elektraOpen", config, errorKey);

error_print:
	if (!lua_isnil (data->L, -1)) ELEKTRA_SET_INSTALLATION_ERROR (errorKey, lua_tostring (data->L, -1));
error:
	/* destroy lua */
	Lua_Shutdown (data->L);
	delete data;
	return -1;
}

int elektraLuaClose (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	if (data == NULL) return 0;

	int ret = Lua_CallFunction_Helper1 (data->L, "elektraClose", errorKey);

	/* destroy lua */
	Lua_Shutdown (data->L);
	delete data;
	return ret;
}

int elektraLuaGet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
#define _MODULE_CONFIG_PATH "system:/elektra/modules/lua"
	if (!strcmp (keyName (parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew (_MODULE_CONFIG_PATH, KEY_VALUE, "lua interpreter waits for your orders", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/get", KEY_FUNC, elektraLuaGet, KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/set", KEY_FUNC, elektraLuaSet, KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/error", KEY_FUNC, elektraLuaError, KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/open", KEY_FUNC, elektraLuaOpen, KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/close", KEY_FUNC, elektraLuaClose, KEY_END),
#include ELEKTRA_README
				     keyNew (_MODULE_CONFIG_PATH "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
	}

	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	if (data != NULL) return Lua_CallFunction_Helper2 (data->L, "elektraGet", returned, parentKey);
	return 0;
}

int elektraLuaSet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	if (data != NULL) return Lua_CallFunction_Helper2 (data->L, "elektraSet", returned, parentKey);
	return 0;
}

int elektraLuaError (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	if (data != NULL) return Lua_CallFunction_Helper2 (data->L, "elektraError", returned, parentKey);
	return 0;
}

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("lua",
		ELEKTRA_PLUGIN_OPEN,  &elektraLuaOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraLuaClose,
		ELEKTRA_PLUGIN_GET,   &elektraLuaGet,
		ELEKTRA_PLUGIN_SET,   &elektraLuaSet,
		ELEKTRA_PLUGIN_ERROR, &elektraLuaError,
		ELEKTRA_PLUGIN_END);
}
}

