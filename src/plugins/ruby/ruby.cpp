/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef SWIG_TYPE_TABLE
#error Build system error, SWIG_TYPE_TABLE is not defined
#endif

#include <ruby.h>

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include <internal/macros/old_utils.h>

#include SWIG_RUNTIME
#include "ruby.hpp"

#include <key.hpp>
#include <keyset.hpp>

/* for global variable access */
#include <mutex>

#include <elektra/kdb/errors.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>

#include <stdarg.h>

using namespace ckdb;

typedef struct _moduleData
{
	VALUE rbInstance = Qnil;

} moduleData;


/*
 * CPP helper function to create new Ruby objects
 */

/* create a new Ruby Kdb::Key object from the given ckdb::Key
 * will be deleted by the Ruby gc. */
static inline VALUE newRubyObject (ckdb::Key * key)
{
	return SWIG_NewPointerObj (new kdb::Key (key), SWIG_TypeQuery ("kdb::Key *"), 1);
}

/* create a new Ruby object and take ownership of this object, thus given keySet will be deleted by
 * the Ruby gc. */
static inline VALUE newRubyObject (kdb::KeySet * keySet)
{
	return SWIG_NewPointerObj (keySet, SWIG_TypeQuery ("kdb::KeySet *"), 1);
}

/*
 * Plugin global variables
 *
 * Although global variables are not allowed (and are not of good style)
 * this is required here.
 * Write access to these variables is guarded by a mutex
 */

/* Plugin instance created by the Ruby-plugin */
static VALUE global_plugin_instance = Qnil;

/* mutex to guard write access to global variables */
static std::mutex global_context_mutex;

#define CONFIG_KEY_SCRIPT "user:/script"

/* global ruby variable: array of plugin instances to prevent them from being gc'ed */
#define RB_GLOBAL_VAR_PLUGINS "Kdb_ruby_plugins"
#define RB_GLOBAL_PLUGIN_KLASS "Kdb_Plugin_klass"
#define RB_GLOBAL_KDB_MODULE "Kdb_Module_klass"

extern "C" {


/**
 * @brief generate string representation of exception
 */
static VALUE get_exception_string (VALUE exception)
{

	/* get backtrace array and join it to a string with '\n  ' */
	ID bt_id = rb_intern ("backtrace");
	if (rb_respond_to (exception, bt_id))
	{
		VALUE backtrace = rb_funcall (exception, bt_id, 0);
		backtrace = rb_ary_join (backtrace, rb_str_new_cstr ("\n  "));

		return rb_sprintf ("Ruby Exception: %" PRIsVALUE ": %" PRIsVALUE " \n  %" PRIsVALUE, CLASS_OF (exception), exception,
				   backtrace);
	}
	else
	{
		return rb_sprintf ("Ruby Exception: %" PRIsVALUE ": %" PRIsVALUE, CLASS_OF (exception), exception);
	}
}

/**
 * @brief clear ruby exception, log and return it
 *
 * this method should be called directly after rb_protect (or similar function), iff
 * that call issued an exception. This will clear the exception and log a warning
 * message.
 * (inline, to have better logs)
 */
static inline VALUE clear_ruby_exception ()
{
	VALUE exception = rb_errinfo ();
	rb_set_errinfo (Qnil);

#ifdef HAVE_LOGGER
	VALUE msg = get_exception_string (exception);
	ELEKTRA_LOG_WARNING ("%s", StringValueCStr (msg));
#endif
	return exception;
}

/**
 * @brief additional to 'clear_ruby_exception()' adds exception msg to warningsKey
 *
 */
static VALUE clear_ruby_exception_add_warning (ckdb::Key * warningsKey)
{
	VALUE exception = clear_ruby_exception ();
	VALUE msg = get_exception_string (exception);

	ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (warningsKey, StringValueCStr (msg));

	return exception;
}

static VALUE clear_ruby_exception_set_error (ckdb::Key * errorKey)
{
	VALUE exception = clear_ruby_exception ();
	VALUE msg = get_exception_string (exception);

	ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (errorKey, StringValueCStr (msg));

	return exception;
}

#define RUBY_INT_OR_DEFAULT(x) (RB_TYPE_P (x, T_FIXNUM) ? NUM2INT (x) : 1)

/*
 * Ruby instance method call mechanism with exception handling
 */
/* maximum number of arguments (avoids allocating VALUE array for rb_funcall2) */
#define MAX_ARGS 3

/**
 * @brief wrapper function to be called by rb_protect
 *
 * takes a Ruby array with arguments, method id and instance (in that order)
 * to be called.
 */
static VALUE protected_ruby_call_wrapper (VALUE args)
{
	VALUE instance = rb_ary_pop (args);
	ID method = SYM2ID (rb_ary_pop (args));

	const int n = RARRAY_LEN (args) > MAX_ARGS ? MAX_ARGS : RARRAY_LEN (args);
	VALUE _args[MAX_ARGS];
	for (int i = n - 1; 0 <= i; i--)
	{
		_args[i] = rb_ary_pop (args);
	}

	return rb_funcall2 (instance, method, n, _args);
}

/**
 * @brief wrapper function to rb_protect with variable argument list
 *
 * calls method 'method' of Ruby instance 'instance' with given arguments, whereas
 * 'nargs' defines the number of given arguments (all arguments have to be of type
 * 'VALUE').
 * 'state' will be set to a non-zero value, iff an exception was thrown (compare
 * rb_protect), otherwise 'state' will be set to 0.
 */
static VALUE my_rb_protect (VALUE instance, ID method, int * state, int nargs, ...)
{
	va_list ap;
	int i = 0;
	VALUE array;

	ELEKTRA_LOG_DEBUG ("call Plugin.%s with %d argument%s", rb_id2name (method), nargs, nargs > 1 ? "s" : "");
	ELEKTRA_ASSERT (nargs <= MAX_ARGS, "max number of arguments for wrapped ruby call reached, increase MAX_ARGS");

	array = rb_ary_new2 (nargs + 2); // num arguments + instance + method

	va_start (ap, nargs);
	for (i = 0; i < nargs; i++)
	{
		rb_ary_push (array, va_arg (ap, VALUE));
	}
	va_end (ap);

	rb_ary_push (array, ID2SYM (method));
	rb_ary_push (array, instance);

	return rb_protect (protected_ruby_call_wrapper, array, state);
}

static VALUE rb_kdb_plugin_define (VALUE self ELEKTRA_UNUSED, VALUE name);

/**
 * @brief: define the Kdb::Plugin class
 */
static VALUE define_kdb_plugin_class ()
{
	VALUE module = Qnil;
	VALUE klass = Qnil;

	module = rb_define_module ("Kdb");
	if (!rb_const_defined (rb_cObject, rb_intern (RB_GLOBAL_KDB_MODULE)))
	{
		rb_define_const (rb_cObject, RB_GLOBAL_KDB_MODULE, module);
	}

	klass = rb_define_class_under (module, "Plugin", rb_cObject);
	if (!rb_const_defined (rb_cObject, rb_intern (RB_GLOBAL_PLUGIN_KLASS)))
	{
		rb_define_const (rb_cObject, RB_GLOBAL_PLUGIN_KLASS, klass);
	}

	rb_define_singleton_method (klass, "define", ((VALUE (*) (...)) rb_kdb_plugin_define), 1);
	return klass;
}


/**
 * @brief Kdb::Plugin.define(name): called by Ruby-plugin code to define a new plugin
 *
 * create a new Ruby-plugin
 */
static VALUE rb_kdb_plugin_define (VALUE self, VALUE name)
{

	if (RB_TYPE_P (name, T_SYMBOL))
	{
		// sym2name() not in 1.9
		name = rb_funcall (name, rb_intern ("to_s"), 0);
	}

	ELEKTRA_LOG ("creating new Ruby plugin plugin '%s'", StringValueCStr (name));

	VALUE instance = Qnil;

	instance = rb_funcall (self, rb_intern ("new"), 0);
	if (rb_block_given_p ())
	{
		/* call the given block in the context of the newly created instance */
		VALUE block = rb_block_proc ();
		rb_funcall_with_block (instance, rb_intern ("instance_eval"), 0, NULL, block);
	}
	else
	{
		rb_raise (rb_eArgError, "a block is required");
	}

	rb_iv_set (instance, "@plugin_name", name);

	/* store this plugin instance in our global variable */
	global_plugin_instance = instance;
	ELEKTRA_LOG_DEBUG ("Plugin called Kdb::Plugin.define, name: %s\n", StringValueCStr (name));

	return Qnil;
}

/* ensure this Ruby instance is not garbage collected
 * we simply put them in a global constant array, so the GC finds them and doesn't delete them
 */
static void add_plugin_instance (VALUE instance)
{
	ELEKTRA_LOG_DEBUG ("adding plugin instance to global plugins array: %ld", instance);
	VALUE ary = rb_const_get (rb_cObject, rb_intern (RB_GLOBAL_VAR_PLUGINS));
	rb_ary_push (ary, instance);
}

/* remove this instance from our global plugins array, now this instance can be GCed */
// static void remove_plugin_instance (VALUE instance)
// {
// 	ELEKTRA_LOG_DEBUG ("removing plugin instance to global plugins array: %ld", instance);
// 	VALUE ary = rb_const_get (rb_cObject, rb_intern (RB_GLOBAL_VAR_PLUGINS));
// 	rb_funcall (ary, rb_intern ("delete"), 1, instance);
// }


static VALUE require_kdb (VALUE v ELEKTRA_UNUSED)
{
	rb_require ("kdb");
	return Qnil;
}


static int init_ruby_environment (ckdb::Key * warningsKey)
{
	/*
	 * init and start Ruby-VM
	 * does nothing, if it is already running
	 */
	ELEKTRA_LOG ("init and start Ruby-VM");
	if (ruby_setup ())
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (warningsKey, "Could not initialize Ruby-VM");
		return -1;
	}

	/* if the global constant TMP_RUBY_PREFIX is already defined
	 * ruby_init_loadpath() was already called */
	if (!rb_const_defined (rb_cObject, rb_intern ("TMP_RUBY_PREFIX")))
	{
		ELEKTRA_LOG ("init Ruby environment");

		ruby_init_loadpath ();

		/* NOT REQUIRED HERE -- define Plugin class */
		// VALUE klass_Plugin = define_kdb_plugin_class();
		// rb_require ("kdb");

		/* define our global plugins array: here we collect all active ruby plugin instances */
		if (!rb_const_defined (rb_cObject, rb_intern (RB_GLOBAL_VAR_PLUGINS)))
		{
			rb_define_const (rb_cObject, RB_GLOBAL_VAR_PLUGINS, rb_ary_new ());
		}
	}

	int state = 0;
	rb_protect (require_kdb, Qnil, &state);
	if (state)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (warningsKey, "Could not load Ruby module 'kdb'");
		return -1;
	}

	return 1;
}

static VALUE load_ruby_plugin (VALUE config ELEKTRA_UNUSED)
{

	kdb::KeySet * conf = nullptr;
	/* get kdb::KeySet pointer from Ruby object */
	if (SWIG_ConvertPtr (config, (void **) &conf, SWIG_TypeQuery ("kdb::KeySet *"), 0) == -1)
	{
		/* failed to get pointer */
		ELEKTRA_LOG_WARNING ("could not convert plugin config");
		return Qnil;
	}


	/* check if user supplied a plugin script,
	 * do not issue an error here, otherwise a 'kdb plugin-info ruby' will print a lot of error messages
	 */
	kdb::Key script_key = conf->lookup (CONFIG_KEY_SCRIPT);
	if (!script_key)
	{
		// don't be too verbose here
		// ELEKTRA_LOG_WARNING("no 'script' plugin config defined");
		return Qnil;
	}

	std::string script = script_key->getString ();

	ELEKTRA_LOG ("load Ruby-plugin '%s'", script.c_str ());

	VALUE rb_script = rb_str_new_cstr (script.c_str ());

	/* be sure we have the Kdb::Plugin class with method 'define' */
	define_kdb_plugin_class ();
	/* load the given script */
	rb_load (rb_script, 0);

	return Qnil;
}

/*
 *
 * Elektra plugin API functions
 *
 */


int RUBY_PLUGIN_FUNCTION (CheckConf) (ckdb::Key * errorKey, ckdb::KeySet * conf)
{

	ELEKTRA_LOG_DEBUG ("ruby plugin checkConf");

	/*
	 * check if given Ruby plugin script exists, done by
	 *  - try to load the plugin
	 *  - if the plugin defines a 'check_conf', pass the check to the plugin
	 */
	if (!ksLookupByName (conf, CONFIG_KEY_SCRIPT, 0))
	{
		/* no script specified
		 * do not issue an error or 'kdb plugin-info ruby' causes problems */
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "No 'script' config value specified");
		return -1;
	}

	VALUE config_instance = Qnil;
	/*
	 * create fresh keySet, since the kdb::KeySet takes ownership and deletes the ks
	 * once the RubyVM GC deletes the config object
	 */
	VALUE config = newRubyObject (new kdb::KeySet (ksDup (conf)));

	global_context_mutex.lock ();
	global_plugin_instance = Qnil;

	int state;
	rb_protect (load_ruby_plugin, config, &state);
	if (state)
	{
		global_context_mutex.unlock ();
		clear_ruby_exception_set_error (errorKey);

		return -1;
	}

	if (global_plugin_instance == Qnil)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERROR (errorKey, "Invalid Ruby plugin. Plugin did not call Kdb::Plugin.define");

		global_context_mutex.unlock ();
		return -1;
	}

	config_instance = global_plugin_instance;
	global_context_mutex.unlock ();

	/* check if plugin has a 'check_conf' method and call it */
	ID mConfCheck = rb_intern ("check_conf");
	if (rb_respond_to (config_instance, mConfCheck))
	{
		int exception = 0;
		VALUE ret = my_rb_protect (config_instance, mConfCheck, &exception, 2, newRubyObject (errorKey), config);
		if (exception)
		{
			clear_ruby_exception_set_error (errorKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT (ret);
	}

	/* its OK, if plugin has no 'check_conf' method */
	return 0;
}

int RUBY_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * warningsKey)
{
	/*
	 * parse plugin config settings
	 */
	ELEKTRA_LOG_DEBUG ("ruby plugin open");

	/*
	 * setup data structure and start Ruby VM
	 */
	global_context_mutex.lock ();

	if (init_ruby_environment (warningsKey) != 1)
	{
		global_context_mutex.unlock ();
		ELEKTRA_LOG_WARNING ("could not init ruby environment");
		return 0;
	}

	ckdb::KeySet * conf_ks = elektraPluginGetConfig (handle);
	if (!ksLookupByName (conf_ks, CONFIG_KEY_SCRIPT, 0))
	{
		/* no script specified
		 * do not issue an error or 'kdb plugin-info ruby' causes problems */
		global_context_mutex.unlock ();
		ELEKTRA_LOG_DEBUG ("no 'script' config option specified");
		return 0;
	}
	/*
	 * create fresh keySet, since the kdb::KeySet takes ownership and deletes the ks
	 * once the RubyVM GC deletes the config object
	 */
	VALUE config = newRubyObject (new kdb::KeySet (ksDup (conf_ks)));

	global_plugin_instance = Qnil;
	int state;
	rb_protect (load_ruby_plugin, config, &state);
	if (state)
	{
		global_context_mutex.unlock ();
		clear_ruby_exception_add_warning (warningsKey);
		ELEKTRA_LOG_DEBUG ("failed to load the ruby plugin");

		/* if we return -1, the module is unloaded and the Ruby VM crashes :( */
		return 0;
	}


	if (global_plugin_instance == Qnil)
	{
		global_context_mutex.unlock ();

		/* error, the Ruby-plugin did not call Kdb::Plugin.define
		 * so we do not have a Plugin instance */
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (warningsKey, "Error in Ruby-plugin, didn't call Kdb::Plugin.define");

		return 0;
	}

	ELEKTRA_LOG_DEBUG ("have new Ruby-plugin: %s", rb_obj_classname (global_plugin_instance));

	/*
	 * store data in plugin handle
	 */
	moduleData * data = new moduleData ();
	data->rbInstance = global_plugin_instance;
	add_plugin_instance (data->rbInstance);

	global_context_mutex.unlock ();

	elektraPluginSetData (handle, data);
	/*
	 * pass open call to ruby plugin class
	 */
	ID mOpen = rb_intern ("open");
	if (rb_respond_to (data->rbInstance, mOpen))
	{
		int exception = 0;
		VALUE ret = my_rb_protect (data->rbInstance, mOpen, &exception, 1, newRubyObject (warningsKey));
		if (exception)
		{
			clear_ruby_exception_add_warning (warningsKey);
			ELEKTRA_LOG_WARNING ("exception during Plugin.open");
			// TODO
			return 0;
		}
		return RUBY_INT_OR_DEFAULT (ret);
	}

	return 0;
}


int RUBY_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * warningsKey)
{
	int returnValue = 0;

	ELEKTRA_LOG_DEBUG ("ruby plugin close");

	/*
	 * first pass call to ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	ID method = rb_intern ("close");
	int state = 0;
	VALUE ret = Qnil;

	// if this plugin is used via the ruby bindings, the Ruby VM crashes during
	// 'finalize' here. Maybe we can't all the plugin.close method any more ???
	//
	// if (data != nullptr && rb_respond_to(data->rbInstance, method)) {
	// 	VALUE msg = get_exception_string(data->rbInstance);
	// 	printf("at clone we have instance: %s\n", StringValueCStr(msg));
	// 	ret = my_rb_protect(data->rbInstance, method, &state, 1,
	// 			newRubyObject(warningsKey)
	// 			);
	//	remove_plugin_instance(data->rbInstance);
	// 	if (state) {
	// 		clear_ruby_exception_add_warning(warningsKey);

	// 		returnValue = -1;
	// 		goto free_and_clear;
	// 	}

	// 	returnValue = RUBY_INT_OR_DEFAULT(ret);
	// 	goto free_and_clear;
	// }
	//
	// free_and_clear:
	/*
	 * delete plugin data structure
	 */
	delete data;

	/* TODO
	 * if we can determine, that this RubyVM is the only one, destroy it, otherwise leave it
	 */
	return returnValue;
}


int RUBY_PLUGIN_FUNCTION (Get) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	ELEKTRA_LOG_DEBUG ("ruby plugin get");
	/* TODO
	 * how should we proceed here?
	 * shall we get these values from the according ruby plugin ???
	 * or just add static values from this 'static' plugin ???
	 */

#define _MODULE_CONFIG_PATH "system:/elektra/modules/" RUBY_PLUGIN_NAME_STR
	if (!strcmp (keyName (parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew (_MODULE_CONFIG_PATH, KEY_VALUE, "Ruby plugin", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports", KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/get", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Get), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/set", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Set), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/error", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Error), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/open", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Open), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/close", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Close), KEY_END),
				     keyNew (_MODULE_CONFIG_PATH "/exports/checkconf", KEY_FUNC, RUBY_PLUGIN_FUNCTION (CheckConf), KEY_END),
#include ELEKTRA_README
				     keyNew (_MODULE_CONFIG_PATH "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
		return 0;
	}

	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	ID method = rb_intern ("get");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to (data->rbInstance, method))
	{
		// this keySet will be deleted by the Ruby GC
		kdb::KeySet * wrap_returned = new kdb::KeySet (returned);
		ret = my_rb_protect (data->rbInstance, method, &state, 2, newRubyObject (wrap_returned), newRubyObject (parentKey));
		// release ks ownership
		wrap_returned->release ();
		if (state)
		{
			clear_ruby_exception_set_error (parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT (ret);
	}
	else
	{
		/* if not 'get' method is available, this plugin is useless, therefore set and error */
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Plugin does not have a 'get' method");
		return -1;
	}
	return -1;
}

int RUBY_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	ID method = rb_intern ("set");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to (data->rbInstance, method))
	{
		kdb::KeySet * wrap_returned = new kdb::KeySet (returned);
		ret = my_rb_protect (data->rbInstance, method, &state, 2, newRubyObject (wrap_returned), newRubyObject (parentKey));
		wrap_returned->release ();
		if (state)
		{
			clear_ruby_exception_set_error (parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT (ret);
	}
	/* no plugin data or method not implemented */
	return -1;
}

int RUBY_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	ID method = rb_intern ("error");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to (data->rbInstance, method))
	{
		kdb::KeySet * wrap_returned = new kdb::KeySet (returned);
		ret = my_rb_protect (data->rbInstance, method, &state, 2, newRubyObject (wrap_returned), newRubyObject (parentKey));
		wrap_returned->release ();
		if (state)
		{
			clear_ruby_exception_add_warning (parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT (ret);
	}
	return -1;
}


ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(RUBY_PLUGIN_NAME_STR,
		ELEKTRA_PLUGIN_OPEN,  &RUBY_PLUGIN_FUNCTION(Open),
		ELEKTRA_PLUGIN_CLOSE, &RUBY_PLUGIN_FUNCTION(Close),
		ELEKTRA_PLUGIN_GET,   &RUBY_PLUGIN_FUNCTION(Get),
		ELEKTRA_PLUGIN_SET,   &RUBY_PLUGIN_FUNCTION(Set),
		ELEKTRA_PLUGIN_ERROR, &RUBY_PLUGIN_FUNCTION(Error),
		ELEKTRA_PLUGIN_END);
}
}
