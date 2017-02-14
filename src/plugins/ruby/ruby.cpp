/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef SWIG_TYPE_TABLE
#error Build system error, SWIG_TYPE_TABLE is not defined
#endif

#include <ruby.h>

#ifndef HAVE_KDBCONFIG
#include <kdbconfig.h>
#endif

#include SWIG_RUNTIME
#include "ruby.hpp"

#include <key.hpp>
#include <keyset.hpp>

/* for global variable access */
#include <mutex>

using namespace ckdb;
#include <kdberrors.h>
#include <kdbassert.h>
#include <kdblogger.h>

#include <stdarg.h>

typedef struct
{
	VALUE rbInstance = Qnil;

} moduleData;


/*
 * CPP helper function to create new Ruby objects
 */

static inline VALUE newRubyObject(ckdb::Key * key) {
	return SWIG_NewPointerObj(new kdb::Key(key), SWIG_TypeQuery("kdb::Key *"), 1);
}

static inline VALUE newRubyObject(ckdb::KeySet * keySet) {
	return SWIG_NewPointerObj(new kdb::KeySet(keySet), SWIG_TypeQuery("kdb::KeySet *"), 1);
}

/*
 * Plugin global variables
 *
 * Although global variables are not allowed (and are not of good style)
 * this is required here.
 * Write access to these variables is guarded by a mutex
 */

/* Ruby module and class pointer: Kdb::Plugin */
static VALUE module_Kdb = Qnil;
static VALUE klass_Plugin = Qnil;

/* Plugin instance created by the Ruby-plugin */
static VALUE tmp_instance = Qnil;
/* Plugin configuration */
static VALUE tmp_config = Qnil;

/* mutex to guard write access to global variables */
static std::mutex global_context_mutex;


extern "C" {

/**
 * @brief generate string representation of exception
 */
static VALUE get_exception_string(VALUE exception) {

	/* get backtrace array and join it to a string with '\n  ' */
	VALUE backtrace = rb_funcall(exception, rb_intern("backtrace"), 0);
	backtrace = rb_ary_join(backtrace, rb_str_new_cstr("\n  "));

	return rb_sprintf("Ruby Exception: %"PRIsVALUE": %"PRIsVALUE" \n  %"PRIsVALUE,
			CLASS_OF(exception), exception, backtrace);
}

/**
 * @brief clear ruby exception, log and return it
 *
 * this method should be called directly after rb_protect (or similar function), iff
 * that call issued an exception. This will clear the exception and log a warning 
 * message.
 * (inline, to have better logs)
 */
static inline VALUE clear_ruby_exception() {
	VALUE exception = rb_errinfo();
	rb_set_errinfo(Qnil);

#ifdef HAVE_LOGGER
	VALUE msg = get_exception_string(exception);
	ELEKTRA_LOG_WARNING("%s", StringValueCStr(msg));
#endif
	return exception;
}

/**
 * @brief additional to 'clear_ruby_exception()' addes exception msg to warningsKey
 *
 */
static VALUE clear_ruby_exception_add_warning(ckdb::Key * warningsKey) {
	VALUE exception = clear_ruby_exception();
	VALUE msg = get_exception_string(clear_ruby_exception());

	ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_RUBY_GEN_WARN, warningsKey, StringValueCStr(msg));

	return exception;
}

static VALUE clear_ruby_exception_set_error(ckdb::Key * errorKey) {
	VALUE exception = clear_ruby_exception();
	VALUE msg = get_exception_string(clear_ruby_exception());

	ELEKTRA_SET_ERROR (ELEKTRA_ERROR_RUBY_GEN_ERROR, errorKey, StringValueCStr(msg));

	return exception;
}

#define RUBY_INT_OR_DEFAULT(x)  (RB_TYPE_P(x, T_FIXNUM) ? NUM2INT(x) : 1)

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
static VALUE protected_ruby_call_wrapper(VALUE args) {
	VALUE instance = rb_ary_pop(args);
	ID method = SYM2ID(rb_ary_pop(args));

	const int n = RARRAY_LEN(args) > MAX_ARGS ? MAX_ARGS : RARRAY_LEN(args);
	VALUE _args[MAX_ARGS];
	for (int i = n - 1; 0 <= i; i--) {
		_args[i] = rb_ary_pop(args);
	}

	return rb_funcall2(instance, method, n, _args);
}

/**
 * @brief wrapper function to rb_protect with variable argument list
 *
 * calls method 'method' of Ruby instance 'instance' with given arguments, whereas
 * 'nargs' defines the number of given arguments (all arguments have to be of type
 * 'VALUE').
 * 'state' will be set to a non-zero value, iff an excpetion was thrown (compare
 * rb_protect), otherwise 'state' will be set to 0.
 */
static VALUE my_rb_protect(VALUE instance, ID method, int * state, int nargs, ...) {
	va_list ap;
	int i = 0;
	VALUE array;

	ELEKTRA_LOG_DEBUG("call Plugin.%s with %d argument%s", 
			rb_id2name(method), nargs, nargs > 1 ? "s" : "");
	ELEKTRA_ASSERT(nargs <= MAX_ARGS, "max number of arguments for wrapped ruby call reached, increase MAX_ARGS");

	array = rb_ary_new2(nargs + 2);  // num arguments + instance + method

	va_start(ap, nargs);
	for (i = 0; i < nargs; i++) {
		rb_ary_push(array, va_arg(ap, VALUE));
	}
	va_end(ap);

	rb_ary_push(array, ID2SYM(method));
	rb_ary_push(array, instance);

	return rb_protect(protected_ruby_call_wrapper, array, state);
}

/*
 *
 * Elektra plugin API functions
 *
 */


int RUBY_PLUGIN_FUNCTION (CheckConf) (ckdb::Key * errorKey ELEKTRA_UNUSED, ckdb::KeySet * conf ELEKTRA_UNUSED) {
	/*
	 * TODO implement this
	 */

	/* 
	 * check if given Ruby plugin script exists
	 */

	/*
	 * if possible, start pass this call to the Ruby plugin
	 * requires Plugin allocation ....
	 */
	return 1;
}

/**
 * @brief Kdb::Plugin.define(name): called by Ruby-plugin code to define a new plugin
 *
 * create a new Ruby-plugin
 */
static VALUE rb_kdb_plugin_define(VALUE self ELEKTRA_UNUSED, VALUE name) {

	if (RB_TYPE_P(name, T_SYMBOL)) {
		// sym2name() not in 1.9
		name = rb_funcall(name, rb_intern("to_s"), 0);
	}
	
	ELEKTRA_LOG("creating new Ruby plugin plugin '%s'", 
			StringValueCStr(name));

	VALUE instance = Qnil;

	// TODO, what to do with 'name', store somewhere ?

	/* create new Kdb::Plugin object */
	instance = rb_funcall(klass_Plugin, rb_intern("new"), 0);
	if (rb_block_given_p()) {
		/* call the given block in the context of the newly created instance */
		VALUE block = rb_block_proc();
		rb_funcall_with_block(instance, rb_intern("instance_eval"), 0, NULL, block);
	} else {
		rb_raise(rb_eArgError, "a block is required");
	}

	tmp_instance = instance;

	return Qnil;
}

static int init_ruby_environment(ckdb::Key * warningsKey) {
	/*
	 * init and start Ruby-VM
	 * does nothing, if it is already runngin
	 */
	ELEKTRA_LOG("init and start Ruby-VM");
	if (ruby_setup()) {
		ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_RUBY_GEN_WARN,  warningsKey, 
				"could not initialize Ruby-VM");
		return -1;
	}

	/* if the global constant TMP_RUBY_PREFIX is already defined
	 * ruby_init_loadpath() was already called */
	if ( ! rb_const_defined(rb_cObject, rb_intern("TMP_RUBY_PREFIX"))) {
		ELEKTRA_LOG("init Ruby environment");

		ruby_init_loadpath();

		/* define Plugin class, if not already */
		if (module_Kdb == Qnil && klass_Plugin == Qnil) {
			rb_require("kdb");

			module_Kdb = rb_define_module("Kdb");
			klass_Plugin = rb_define_class_under(module_Kdb, "Plugin", rb_cObject);
			rb_define_singleton_method(klass_Plugin, "define", ((VALUE (*)(...)) rb_kdb_plugin_define), 1);
		}
	}
	return 1;
}

static VALUE load_ruby_plugin(VALUE config ELEKTRA_UNUSED) {
	// TODO make configurable
	// TODO search path ???

	char script_name[] = "rb_hello.rb";

	ELEKTRA_LOG("load Ruby-plugin '%s'", script_name);

	VALUE script = rb_str_new_cstr(script_name);
	/* load the given script */
	rb_load(script, 0);

	return Qnil;
}


int RUBY_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * warningsKey)
{
	/*
	 * parse plugin config settings
	 */

	/*
	 * setup data structure and start Ruby VM
	 */
	global_context_mutex.lock();
	tmp_instance = Qnil;
	tmp_config = Qnil;

	if (init_ruby_environment(warningsKey) != 1) {
		global_context_mutex.unlock();
		return -1;
	}

	VALUE config = newRubyObject(elektraPluginGetConfig(handle));
	tmp_config = config;

	int state;
	rb_protect(load_ruby_plugin, config, &state);
	if (state) {
		global_context_mutex.unlock();
		clear_ruby_exception_add_warning(warningsKey);

		/* if we return -1, the module is unloaded and the Ruby VM crashes :( */
		return 0;
	}
	

	if (tmp_instance == Qnil) {
		global_context_mutex.unlock();

		/* error, the Ruby-plugin did not call Kdb::Plugin.define
		 * so we do not have a Plugin instance */
		ELEKTRA_ADD_WARNING(ELEKTRA_WARNING_RUBY_GEN_WARN, warningsKey, 
				"Error in Ruby-plugin, didn't call Kdb::Plugin.define");
		
		return 0;
	}

	ELEKTRA_LOG_DEBUG("have new Ruby-plugin: %s", rb_obj_classname(tmp_instance));
	/*
	 * store data in plugin handle
	 */
	moduleData * data = new moduleData();
	data->rbInstance = tmp_instance;
	tmp_instance = Qnil;
	tmp_config = Qnil;

	global_context_mutex.unlock();

	elektraPluginSetData (handle, data);
	/*
	 * pass open call to ruby plugin class
	 */
	ID mOpen = rb_intern("open");
	if (rb_respond_to(data->rbInstance, mOpen)) {
		int exception = 0;
		VALUE ret = my_rb_protect(data->rbInstance, mOpen, &exception, 1,
				newRubyObject(warningsKey)
				);
		if (exception) {
			clear_ruby_exception_add_warning(warningsKey);
			// TODO
			return 0;
		}
		return RUBY_INT_OR_DEFAULT(ret);
	}

	return 0;
}


int RUBY_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * warningsKey)
{
	int returnValue = 0;
	/* 
	 * first pass call to ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData(handle));
	ID method = rb_intern("close");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to(data->rbInstance, method)) {
		ret = my_rb_protect(data->rbInstance, method, &state, 1,
				newRubyObject(warningsKey)
				);
		if (state) {
			clear_ruby_exception_add_warning(warningsKey);

			returnValue = -1;
			goto free_and_clear;
		}

		returnValue = RUBY_INT_OR_DEFAULT(ret);
		goto free_and_clear;
	}

free_and_clear:
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
	/* TODO 
	 * how should we proceed here?
	 * shall we get these values from the according ruby plugin ???
	 * or just add static values from this 'static' plugin ???
	 */

#define _MODULE_CONFIG_PATH "system/elektra/modules/" RUBY_PLUGIN_NAME_STR
	if (!strcmp (keyName (parentKey), _MODULE_CONFIG_PATH))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, 
				  keyNew (_MODULE_CONFIG_PATH, KEY_VALUE, "Ruby plugin", KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports", KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/get", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Get), KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/set", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Set), KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/error", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Error), KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/open", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Open), KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/close", KEY_FUNC, RUBY_PLUGIN_FUNCTION (Close), KEY_END),
				  keyNew (_MODULE_CONFIG_PATH "/exports/checkconf", KEY_FUNC, RUBY_PLUGIN_FUNCTION (CheckConf), KEY_END),
#include ELEKTRA_README (RUBY_PLUGIN_NAME)
				  keyNew (_MODULE_CONFIG_PATH "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
		return 0;
	}

	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData (handle));
	ID method = rb_intern("get");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to(data->rbInstance, method)) {
		ret = my_rb_protect(data->rbInstance, method, &state, 2,
				newRubyObject(returned),
				newRubyObject(parentKey)
				);
		if (state) {
			clear_ruby_exception_set_error(parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT(ret);
	}
	return -1;
}

int RUBY_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData(handle));
	ID method = rb_intern("set");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to(data->rbInstance, method)) {
		ret = my_rb_protect(data->rbInstance, method, &state, 2,
				newRubyObject(returned),
				newRubyObject(parentKey)
				);
		if (state) {
			clear_ruby_exception_set_error(parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT(ret);
	}
	/* no plugin data or method not implemented */
	return -1;
}

int RUBY_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/*
	 * pass call to Ruby plugin
	 */
	moduleData * data = static_cast<moduleData *> (elektraPluginGetData(handle));
	ID method = rb_intern("error");
	int state = 0;
	VALUE ret = Qnil;

	if (data != nullptr && rb_respond_to(data->rbInstance, method)) {
		ret = my_rb_protect(data->rbInstance, method, &state, 2,
				newRubyObject(returned),
				newRubyObject(parentKey)
				);
		if (state) {
			clear_ruby_exception_add_warning(parentKey);
			return -1;
		}
		return RUBY_INT_OR_DEFAULT(ret);
	}
	return -1;
}


ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (RUBY_PLUGIN_NAME)
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
