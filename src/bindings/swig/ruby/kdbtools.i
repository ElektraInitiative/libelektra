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



%include "attribute.i"
%include <std_vector.i>
%include <std_map.i>
%include <std_string.i>
%include "stdint.i"
%include "exception.i"
%include "std_except.i"

/* add mapping for std::bad_alloc exception */
namespace std {
  %std_exception_map(bad_alloc, SWIG_MemoryError);

  %template(VectorStr) vector<string>;
};

%import "kdb.i"

%{
  #include "pluginspec.hpp"
  #include "plugin.hpp"
  #include "plugins.hpp"
  #include "plugindatabase.hpp"

  using namespace kdb::tools;
%}

%apply long { ssize_t }

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
