/**
 * @file
 *
 * @brief Swig interface file for KDB Ruby bindings
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

%feature("autodoc", "3");
/*
%define CPPDOCURL "http://doc.libelektra.org/api/current/html" %enddef

%define DOCSTRING
"This module is a SWIG generated binding for KDB (http://www.libelektra.org),
therefore the module provides wrapper classes to KDBs C++ interface and is
mainly a 1 to 1 relation. However, to provide a more Ruby-style API to KDB,
this module differs to the C++ API in the following way:
 * C++ iterators for Key/KeySet are excluded. Instead KeySet implements
   a 'each' method and includes 'Enumerable'. Therefore it is very similar to
   a Ruby-Array. However, the KeySet cursor methods are still available.
 * Access to native C-level KDB structures (such as ckdb::Key) is not
   possible, as this does not make much sense within Ruby.
 * Method names are renamed to follow Ruby naming conventions
 * Key and KeySet methods directly modify the underlying Key/KeySet

Please note, this documentation will show C++ types too (e.g. std::string).
"
%enddef
*/
/* docstring for module implemented for swig >= 3.0.18 */

%module kdbtools


%include <attribute.i>
%include <std_vector.i>
%include <std_map.i>
%include <std_string.i>
%include <stdint.i>
%include <exception.i>
%include <std_except.i>


namespace std {
/* add mapping for std::bad_alloc exception */
  %std_exception_map(bad_alloc, SWIG_MemoryError);
/* mapping for a vector<string> */
  %template(VectorStr) vector<string>;
};


%{
  #include "pluginspec.hpp"
  #include "plugin.hpp"
  #include "plugins.hpp"
  #include "plugindatabase.hpp"
  #include "modules.hpp"

  using namespace kdb::tools;
%}

/* let swig know about Elektra base types */
%import "kdb.i"

%init {
  /* correct SWIG type <-> Ruby class mappying
   *
   * SWIG creates in the first step, for each known class to map
   * a Ruby class under module SWIG, named 
   *   'TYPE_p_<cpp-namespace><cpp-class-name>'
   * I'm not really sure about its purpose, maybe a fallback?
   * In the second step, SWIG creates all its 'real' Ruby classes
   * under the defined module and updates its type map records, to
   * store this 'real' class (Ruby klass VALUE).
   *
   * The function SWIG_Ruby_NewPointerObj(), given a type_info struct
   * lookups the corresponding type table entry to fetch the 'real'
   * klass Object created earlier. If no such klass Object is found for
   * the corresponding type_info, the fallback klass under SWIG module
   * is used.
   *
   * So far so good, but: The %import kdb.i lets swig know about the
   * kdb::Key and kdb::KeySet types and creates entries in SWIGs type
   * table. However, these type table entries arn't never filled up
   * (more correctly the client data). So for our imported stuff from 
   * 'kdb.i' we will end up with getting these fake 
   * 'SWIG::TYPE_p_kdb__KeyXXX' klasses but not with our real
   * 'Kdb::KeyXXX' klasses.
   *
   * Until now, if not seen any better method of correcting this myself
   * by hand. So here we get our real 'Kdb::*' klasses and update the
   * fallback mechanism (the SWIG modules constants).
   */
  VALUE my_mKdb = Qnil;
  VALUE my_kKey = Qnil;
  VALUE my_kKeySet = Qnil;

  /* get the Kdb module and its classes (they already exist, so they
   * aren't recreated) */
  my_mKdb = rb_define_module("Kdb");
  my_kKey = rb_define_class_under(my_mKdb, "Key", rb_cObject);
  my_kKeySet = rb_define_class_under(my_mKdb, "KeySet", rb_cObject);

  /* SWIG (also Ruby?) uses these constants to fetch the Ruby class
   * object, so we simply have to reinitialize these constants */
  /* do a remove first to avoid "const already set" warning */
  rb_const_remove(_mSWIG, rb_intern("TYPE_p_kdb__Key"));
  rb_const_set(_mSWIG, rb_intern("TYPE_p_kdb__Key"), my_kKey);
  rb_const_remove(_mSWIG, rb_intern("TYPE_p_kdb__KeySet"));
  rb_const_set(_mSWIG, rb_intern("TYPE_p_kdb__KeySet"), my_kKeySet);
}


%apply long { ssize_t }

/* for some reason, this does not work, at least for 
 * Modules::load() methods. Maybe because of the overloading ???
%feature("novaluewrapper") kdb::tools::PluginPtr;
%feature("novaluewrapper") std::unique_ptr< kdb::tools::Plugin >;
 */

%header {
  /**
   * SwigValueWrapper specialication for std::unique_ptr
   * the default SwigValueWrapper implementaion does not work here,
   * since unique_ptr::unique_ptr(&unique_ptr) = delete;
   *
   * Therefore we have to create our own SwigValueWrapper for the
   * pointer wrapper unique_ptr (wrapper of wrapper in a wrapper code ;)
   */
  template<>
  class SwigValueWrapper< std::unique_ptr< kdb::tools::Plugin > > {

    std::unique_ptr<kdb::tools::Plugin> p;

  public:
    SwigValueWrapper() : p(nullptr) { }
    SwigValueWrapper& operator=(const std::unique_ptr<kdb::tools::Plugin>& t) {
      /* transfer ownership from t to p
       * scope of t will end in this function, thus also its pointer will
       * be deleted (Plugin).
       * So we move the pointer (Plugin) from t to p.
       * a 'const_cast' is required here, since 'release()' modifies t
       */
      p.reset( (const_cast<std::unique_ptr<kdb::tools::Plugin>&>(t)).release() );
      return *this;
    }

    std::unique_ptr<kdb::tools::Plugin> *operator&() {
      return &p;
    }
  };
}

%ignore kdb::tools::PluginPtr;
%ignore std::unique_ptr< kdb::tools::Plugin >;

/*************************************************************************
 *
 * kdb::tools::PluginSpec
 *
 ************************************************************************/

/* %predicate kdb::tools::PluginSpec::isRefNumber; */

/* getter/setter */
%rename("fullname") kdb::tools::PluginSpec::getFullName;
%rename("fullname=") kdb::tools::PluginSpec::setFullName;

%rename("name") kdb::tools::PluginSpec::getName;
%rename("name=") kdb::tools::PluginSpec::setName;

%rename("config") kdb::tools::PluginSpec::getConfig;
%rename("config=") kdb::tools::PluginSpec::setConfig;

%rename("refname") kdb::tools::PluginSpec::getRefName;
%rename("refname=") kdb::tools::PluginSpec::setRefName;

%rename("refnumber=") kdb::tools::PluginSpec::setRefNumber;
%rename("is_refnumber?") kdb::tools::PluginSpec::isRefNumber;



%include "pluginspec.hpp"

/*************************************************************************
 *
 * kdb::tools::Plugin
 *
 ************************************************************************/

%ignore kdb::tools::Plugin::getSymbol;
%ignore kdb::tools::Plugin::operator=;


%feature("novaluewrapper") kdb::tools::PluginPtr;
%feature("novaluewrapper") std::unique_ptr< kdb::tools::Plugin >;

/* out typemap for PluginPtr (which is a std::unique_ptr)
 * 
 * The unique_ptr object will delete its wrapped object once its scope
 * ends. But here we want to keep the Plugin object (the unique_ptr does
 * not matter), so we have to do a release() on the unique_ptr object.
 * Otherwise the Plugin is deleted and we end up with a Ruby object holding
 * a pointer to a deleted object. */
%typemap(out) kdb::tools::PluginPtr {
  %set_output(SWIG_NewPointerObj($1.release(), $descriptor(kdb::tools::Plugin *), SWIG_POINTER_OWN | 0));
}

/*
 * parse plugin.hpp
 */
%include "plugin.hpp"


/*************************************************************************
 *
 * kdb::tools::Plugins
 *
 ************************************************************************/

%include "plugins.hpp"


/*************************************************************************
 *
 * kdb::tools::PluginDatabase
 *
 ************************************************************************/

%ignore kdb::tools::PluginDatabase::getSymbol;
%ignore kdb::tools::ModulesPluginDatabase::getSymbol;
%ignore kdb::tools::MockPluginDatabase::getSymbol;

%template(IntPluginSpecMap) std::map<int, kdb::tools::PluginSpec>;

%constant const int PLUGIN_STATUS_PROVIDES =
        kdb::tools::PluginDatabase::Status::provides;
%constant const int PLUGIN_STATUS_REAL =
        kdb::tools::PluginDatabase::Status::real;
%constant const int PLUGIN_STATUS_MISSING =
        kdb::tools::PluginDatabase::Status::missing;


%include "plugindatabase.hpp"



/*************************************************************************
 *
 * kdb::tools::Modules
 *
 ************************************************************************/


%include "modules.hpp"


