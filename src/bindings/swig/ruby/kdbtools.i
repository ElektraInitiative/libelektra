/**
 * @file
 *
 * @brief Swig interface file for KDB Ruby bindings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%feature("autodoc", "3");
/*
%define CPPDOCURL "https://doc.libelektra.org/api/latest/html" %enddef

%define DOCSTRING
"This module is a SWIG generated binding for KDB (https://www.libelektra.org),
therefore the module provides wrapper classes to KDBs C++ interface and is
mainly a 1 to 1 relation. However, to provide a more Ruby-style API to KDB,
this module differs to the C++ API in the following way:
 * C++ iterators for Key/KeySet are excluded. Instead KeySet implements
   a 'each' method and includes 'Enumerable'. Therefore it is very similar to
   a Ruby-Array.
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

#pragma SWIG nowarn=378 // Disable warning: operator!= ignored

%include <attribute.i>
%include <std_vector.i>
%include <std_map.i>
%include <std_string.i>
%include <stdint.i>
%include <exception.i>
%include <std_except.i>
%include <std_shared_ptr.i>

/* shared pointer definitions have to be before the first include
 * statements (have to be defined before its types */
#define SWIG_SHARED_PTR_NAMESPACE std
%shared_ptr(kdb::tools::PluginDatabase)
/* MUST be done for all sub-types too !!! */
%shared_ptr(kdb::tools::ModulesPluginDatabase)
%shared_ptr(kdb::tools::PluginVariantDatabase)
%shared_ptr(kdb::tools::MockPluginDatabase)


namespace std {
/* add mapping for std::bad_alloc exception */
  %std_exception_map(bad_alloc, SWIG_MemoryError);
/* mapping for a vector<string> */
  %template(VectorStr) vector<string>;
};


%{
  #include <pluginspec.hpp>
  #include <plugin.hpp>
  #include <plugins.hpp>
  #include <plugindatabase.hpp>
  #include <modules.hpp>
  #include <backendparser.hpp>
  #include <backend.hpp>
  #include <backends.hpp>
  #include <backendbuilder.hpp>
  #include <specreader.hpp>

  #include <toolexcept.hpp>

  using namespace kdb::tools;
%}

/* let swig know about Elektra base types */
%import "kdb.i"

%init {
  /* correct SWIG type <-> Ruby class mapping
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
   *
   * I've filed an issue at https://github.com/swig/swig/issues/903
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

/*************************************************************************
 *
 * kdb::tools:: Exceptions
 *
 ************************************************************************/

%exceptionclass kdb::tools::ToolException;
%rename("to_s") kdb::tools::ToolException::what;

%exceptionclass kdb::tools::ParseException;
%exceptionclass kdb::tools::PluginCheckException;
%exceptionclass kdb::tools::BackendCheckException;
%exceptionclass kdb::tools::FileNotValidException;
%exceptionclass kdb::tools::MountpointInvalidException;
%exceptionclass kdb::tools::MountpointAlreadyInUseException;
%exceptionclass kdb::tools::NoSuchBackend;
%exceptionclass kdb::tools::PluginAlreadyInserted;
%exceptionclass kdb::tools::PluginConfigInvalid;
%exceptionclass kdb::tools::BadPluginName;
%exceptionclass kdb::tools::TooManyPlugins;
%exceptionclass kdb::tools::OrderingViolation;
%exceptionclass kdb::tools::CyclicOrderingViolation;
%exceptionclass kdb::tools::NoPlugin;
%exceptionclass kdb::tools::ReferenceNotFound;
%exceptionclass kdb::tools::MissingNeeded;
%exceptionclass kdb::tools::MissingSymbol;
%exceptionclass kdb::tools::WrongStatus;
%exceptionclass kdb::tools::SymbolMismatch;
%exceptionclass kdb::tools::NoGlobalPlugin;
%exceptionclass kdb::tools::SymbolDuplicate;
%exceptionclass kdb::tools::StoragePlugin;
%exceptionclass kdb::tools::ResolverPlugin;
%exceptionclass kdb::tools::PluginNoContract;
%exceptionclass kdb::tools::PluginNoInfo;
%exceptionclass kdb::tools::VersionInfoMismatch;

%include "toolexcept.hpp"


/* for some reason, this does not work, at least for
 * Modules::load() methods. Maybe because of the overloading ???
%feature("novaluewrapper") kdb::tools::PluginPtr;
%feature("novaluewrapper") std::unique_ptr< kdb::tools::Plugin >;
 */

/* generic SWIG macro for std::unique_ptr types
 * defines a specialization of SwigValueWrapper
 * and a OUT typemap
 */
%define UNIQUE_PTR_VALUE_WRAPPER(PTR_TYPE, TYPE)
%header {
  /**
   * SwigValueWrapper specialization for std::unique_ptr
   * the default SwigValueWrapper implementation does not work here,
   * since unique_ptr::unique_ptr(&unique_ptr) = delete;
   *
   * Therefore we have to create our own SwigValueWrapper for the
   * pointer wrapper unique_ptr (wrapper of wrapper in a wrapper code ;)
   */
  template<>
  class SwigValueWrapper< PTR_TYPE > {

    PTR_TYPE p;

  public:
    SwigValueWrapper() : p(nullptr) { }
    SwigValueWrapper& operator=(const PTR_TYPE& t) {
      /* transfer ownership from t to p
       * scope of t will end in this function, thus also its pointer will
       * be deleted (Plugin).
       * So we move the pointer (Plugin) from t to p.
       * a 'const_cast' is required here, since 'release()' modifies t
       */
      p.reset( (const_cast<PTR_TYPE&>(t)).release() );
      return *this;
    }

    PTR_TYPE *operator&() {
      return &p;
    }
  };
}

/* out typemap for PluginPtr (which is a std::unique_ptr)
 *
 * The unique_ptr object will delete its wrapped object once its scope
 * ends. But here we want to keep the Plugin object (the unique_ptr does
 * not matter), so we have to do a release() on the unique_ptr object.
 * Otherwise the Plugin is deleted and we end up with a Ruby object holding
 * a pointer to a deleted object. */
%typemap(out) PTR_TYPE {
  %set_output(SWIG_NewPointerObj($1.release(), $descriptor(TYPE *), SWIG_POINTER_OWN | 0));
}
%enddef

/* unique_ptr macro usage */
UNIQUE_PTR_VALUE_WRAPPER(
        kdb::tools::PluginPtr, kdb::tools::Plugin)

UNIQUE_PTR_VALUE_WRAPPER(
        kdb::tools::BackendInterfacePtr,
        kdb::tools::BackendInterface)

UNIQUE_PTR_VALUE_WRAPPER(
        kdb::tools::MountBackendInterfacePtr,
        kdb::tools::MountBackendInterface)



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

%catches(kdb::tools::BadPluginName,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginSpec::PluginSpec;

%catches(kdb::tools::BadPluginName,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginSpec::setFullName;

%catches(kdb::tools::BadPluginName,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginSpec::setRefName;

%catches(kdb::tools::BadPluginName,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginSpec::setName;

%catches(kdb::tools::BadPluginName,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginSpec::validate;


%template(PluginSpecVector) std::vector<kdb::tools::PluginSpec>;

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


%catches (
        kdb::tools::NoPlugin,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::Plugin;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::loadInfo;

%catches (
        kdb::tools::PluginNoContract,
        kdb::tools::PluginNoInfo,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::parse;

%catches (
        kdb::tools::VersionInfoMismatch,
        kdb::tools::WrongStatus,
        kdb::tools::SymbolMismatch,
        kdb::tools::SymbolDuplicate,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::check;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::open;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::close;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::get;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::set;

%catches (
        kdb::tools::MissingSymbol,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Plugin::error;


/*
 * parse plugin.hpp
 */
%include "plugin.hpp"


/*************************************************************************
 *
 * kdb::tools::Plugins
 *
 ************************************************************************/

%catches(kdb::tools::TooManyPlugins,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::Plugins::checkPlacement;

%catches(kdb::tools::StoragePlugin,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::Plugins::checkStorage;

%catches(kdb::tools::ResolverPlugin,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::Plugins::checkResolver;

%catches(kdb::tools::OrderingViolation,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::Plugins::checkOrdering;

%catches(kdb::tools::ConflictViolation,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::Plugins::checkConflicts;

%catches(kdb::tools::MissingSymbol,
         kdb::tools::TooManyPlugins,
         kdb::tools::StoragePlugin,
         kdb::tools::ResolverPlugin,
         kdb::tools::OrderingViolation,
         kdb::tools::ConflictViolation,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::ErrorPlugins::tryPlugin;

%catches(kdb::tools::MissingSymbol,
         kdb::tools::TooManyPlugins,
         kdb::tools::StoragePlugin,
         kdb::tools::ResolverPlugin,
         kdb::tools::OrderingViolation,
         kdb::tools::ConflictViolation,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::GetPlugins::tryPlugin;

%catches(kdb::tools::MissingSymbol,
         kdb::tools::TooManyPlugins,
         kdb::tools::StoragePlugin,
         kdb::tools::ResolverPlugin,
         kdb::tools::OrderingViolation,
         kdb::tools::ConflictViolation,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::SetPlugins::tryPlugin;

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

/* TODO, be more explicit if known */
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::listAllPlugins;
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::lookupInfo;
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::lookupProvides;
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::lookupAllProvidesWithStatus;
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::lookupAllProvides;
%catches(kdb::tools::ToolException)
        kdb::tools::PluginDatabase::calculateStatus;
%catches(kdb::tools::NoPlugin,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::PluginDatabase::lookupMetadata;

%catches(kdb::tools::ToolException)
        kdb::tools::PluginVariantDatabase::getPluginVariants;



%include "plugindatabase.hpp"




/*************************************************************************
 *
 * kdb::tools::Modules
 *
 ************************************************************************/

%catches (
        kdb::tools::BadPluginName,
        kdb::tools::NoPlugin,
        kdb::tools::MissingSymbol,
        kdb::tools::PluginNoContract,
        kdb::tools::PluginNoInfo,
        kdb::tools::VersionInfoMismatch,
        kdb::tools::WrongStatus,
        kdb::tools::SymbolMismatch,
        kdb::tools::SymbolDuplicate,
        kdb::tools::PluginCheckException,
        kdb::tools::ToolException
) kdb::tools::Modules::load;


%include "modules.hpp"




/*************************************************************************
 *
 * kdb::tools::parse(Plugin)Arguments
 *
 ************************************************************************/

%ignore kdb::tools::parseArguments(std::initializer_list<std::string>);
%ignore kdb::tools::detail::processArgument;
%ignore kdb::tools::detail::fixArguments;

%catches (kdb::tools::ParseException) kdb::tools::parsePluginArguments;
%catches (kdb::tools::ParseException) kdb::tools::parseArguments;

%include "backendparser.hpp"




/*************************************************************************
 *
 * backend.hpp
 *
 ************************************************************************/

%ignore kdb::tools::operator<<(std::ostream &, Backend const &);
%ignore kdb::tools::Backend::operator=;

/*
 * the void status(std::ostream) methods are useless within Ruby
 * convert them to a std::string status() method
 */
%ignore kdb::tools::MountBackendInterface::status;

%define STATUS_OSTREAM_TO_STRING(TYPE)
%extend TYPE {
  std::string status() {
    std::ostringstream os;
    $self->status(os);
    return os.str();
  }
}
%enddef

STATUS_OSTREAM_TO_STRING(kdb::tools::Backend)
STATUS_OSTREAM_TO_STRING(kdb::tools::ImportExportBackend)


%catches(kdb::tools::NoSuchBackend,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException
) kdb::tools::BackendFactory::create;

%catches(kdb::tools::MountpointAlreadyInUseException,
         kdb::tools::MountpointInvalidException,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException
) kdb::tools::BackendInterface::setMountpoint;

%catches(kdb::tools::MissingSymbol,
         kdb::tools::FileNotValidException,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException
) kdb::tools::MountBackendInterface::useConfigFile;

%catches(kdb::tools::BackendCheckException,
         kdb::tools::PluginCheckException,
         kdb::tools::ToolException
) kdb::tools::BackendInterface::addPlugin;

%catches(kdb::tools::NoGlobalPlugin,
         kdb::tools::BackendCheckException,
         kdb::tools::OrderingViolation,
         kdb::tools::ToolException
) kdb::tools::GlobalPlugins::serialize;

%catches(kdb::tools::TooManyPlugins,
         kdb::tools::OrderingViolation,
         kdb::tools::ToolException,
         ...
) kdb::tools::SerializeInterface::serialize;

%catches(kdb::tools::TooManyPlugins,
         kdb::tools::OrderingViolation,
         kdb::tools::ToolException,
         ...
) kdb::tools::GlobalPluginsBuilder::serialize;


%include "backend.hpp"




/*************************************************************************
 *
 * backends.hpp
 *
 ************************************************************************/

%template(BackendInfoVector) std::vector<kdb::tools::BackendInfo>;

/* ignore this constant, since SWIG treats it as static member variable
 * and defines a setter method for it */
%ignore kdb::tools::Backends::mountpointsPath;

/* is there no way to define a class constant?
 * so this will become Kdbtools::MOUNTPOINTS_PATH
*/
%constant const char * mountpoints_path =
        kdb::tools::Backends::mountpointsPath;

%include "backends.hpp"




/*************************************************************************
 *
 * backendbuilder.hpp
 *
 ************************************************************************/

/* this is just a local helper class */
%ignore kdb::tools::BackendBuilderInit;

%ignore kdb::tools::BackendBuilder::begin;
%ignore kdb::tools::BackendBuilder::end;
%ignore kdb::tools::BackendBuilder::cbegin;
%ignore kdb::tools::BackendBuilder::cend;
%extend kdb::tools::BackendBuilder {
    PluginSpecVector to_add() {
       return PluginSpecVector($self->begin(), $self->end());
    }
}

%rename("backend_config") kdb::tools::BackendBuilder::getBackendConfig;
%rename("backend_config=") kdb::tools::BackendBuilder::setBackendConfig;

%ignore kdb::tools::GlobalPluginsBuilder::globalPluginsPath;
%constant const char * GLOBAL_PLUGINS_PATH =
        kdb::tools::GlobalPluginsBuilder::globalPluginsPath;


%catches(kdb::tools::NoPlugin,
         kdb::tools::CyclicOrderingViolation,
         kdb::tools::PluginAlreadyInserted,
         kdb::tools::PluginConfigInvalid,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException
) kdb::tools::BackendBuilder::addPlugin;

%catches(kdb::tools::NoPlugin,
         kdb::tools::CyclicOrderingViolation,
         kdb::tools::PluginAlreadyInserted,
         kdb::tools::PluginConfigInvalid,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException
) kdb::tools::BackendBuilder::addPlugins;

%catches(kdb::tools::NoPlugin,
         kdb::tools::CyclicOrderingViolation,
         kdb::tools::PluginAlreadyInserted,
         kdb::tools::PluginConfigInvalid,
         kdb::tools::BackendCheckException,
         kdb::tools::ToolException,
         ...
) kdb::tools::BackendBuilder::resolveNeeds;

%catches(kdb::tools::ToolException,
         ...
) kdb::tools::BackendBuilder::fillPlugins;

%catches(kdb::tools::ToolException,
         ...
) kdb::tools::BackendBuilder::fillPlugins;



%include "backendbuilder.hpp"




/*************************************************************************
 *
 * specreader.hpp
 *
 ************************************************************************/
/* std::unordered_map is currently not supported by the
 * SWIG standard library. So we convert it to a std::map
 * and use the SWIG std::map type wrapping.*/
%template(SpecBackendBuilderMap)
        std::map<kdb::Key, kdb::tools::SpecBackendBuilder>;

/* std::unordered_map<Key, SpecBackendBuilder> is just used
 * as return type for SpecReader::getBackends(). Thus we just
 * need a 'out' typemap for this now */
%typemap(out) std::unordered_map<kdb::Key, kdb::tools::SpecBackendBuilder> (std::map<kdb::Key, kdb::tools::SpecBackendBuilder> * tmp_backends) {
        // Backends-typemap
  tmp_backends = new std::map<kdb::Key, kdb::tools::SpecBackendBuilder>(
        $1.begin(), $1.end());
  %set_output(SWIG_NewPointerObj(tmp_backends,
        SWIG_TypeQuery("std::map< kdb::Key, kdb::tools::SpecBackendBuilder > *"),
        1));
}


%include "specreader.hpp"
