/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%module tools

%{
  #include <toolexcept.hpp>
  #include <backend.hpp>
  #include <backends.hpp>
  #include <backendparser.hpp>
  #include <backendbuilder.hpp>
  #include <plugin.hpp>
  #include <plugins.hpp>
  #include <pluginspec.hpp>
  #include <plugindatabase.hpp>
  #include <specreader.hpp>
  #include <modules.hpp>
  using namespace kdb::tools;
%}

%include <stl.i>
%include <std_shared_ptr.i>
%include <std_unordered_map.i>
%import "kdb.i"
%feature("autodoc", "3");

/* handle exceptions */
%{
  #define KDB_TOOLS_CATCH_EX(exception) \
    catch(const kdb::tools::exception &e) \
    { \
      SWIG_Python_Raise(SWIG_NewPointerObj(new kdb::tools::exception(e), \
        SWIGTYPE_p_kdb__tools__##exception, SWIG_POINTER_OWN), \
        #exception, SWIGTYPE_p_kdb__tools__##exception); \
      SWIG_fail; \
    }

  #define BACKEND_EXCEPTIONS \
    KDB_TOOLS_CATCH_EX(FileNotValidException) \
    KDB_TOOLS_CATCH_EX(MountpointInvalidException) \
    KDB_TOOLS_CATCH_EX(MountpointAlreadyInUseException) \
    KDB_TOOLS_CATCH_EX(NoSuchBackend) \
    KDB_TOOLS_CATCH_EX(BackendCheckException) \
    KDB_TOOLS_CATCH_EX(ToolException)

  #define PLUGIN_EXCEPTIONS \
    KDB_TOOLS_CATCH_EX(VersionInfoMismatch) \
    KDB_TOOLS_CATCH_EX(PluginNoInfo) \
    KDB_TOOLS_CATCH_EX(PluginNoContract) \
    KDB_TOOLS_CATCH_EX(ResolverPlugin) \
    KDB_TOOLS_CATCH_EX(StoragePlugin) \
    KDB_TOOLS_CATCH_EX(SymbolDuplicate) \
    KDB_TOOLS_CATCH_EX(NoGlobalPlugin) \
    KDB_TOOLS_CATCH_EX(SymbolMismatch) \
    KDB_TOOLS_CATCH_EX(WrongStatus) \
    KDB_TOOLS_CATCH_EX(MissingSymbol) \
    KDB_TOOLS_CATCH_EX(MissingNeeded) \
    KDB_TOOLS_CATCH_EX(ReferenceNotFound) \
    KDB_TOOLS_CATCH_EX(NoPlugin) \
    KDB_TOOLS_CATCH_EX(ConflictViolation) \
    KDB_TOOLS_CATCH_EX(OrderingViolation) \
    KDB_TOOLS_CATCH_EX(TooManyPlugins) \
    KDB_TOOLS_CATCH_EX(BadPluginName) \
    KDB_TOOLS_CATCH_EX(PluginConfigInvalid) \
    KDB_TOOLS_CATCH_EX(PluginAlreadyInserted) \
    KDB_TOOLS_CATCH_EX(PluginCheckException) \
    KDB_TOOLS_CATCH_EX(ToolException)
%}

#define KDB_CATCH(exceptions) \
  try { \
    $action \
  } \
  exceptions \
  catch (const std::exception & e) { \
    SWIG_exception(SWIG_RuntimeError, e.what()); \
  } \
  catch (...) { \
    SWIG_exception(SWIG_UnknownError, "unknown error in $decl"); \
  }

%pythoncode {
  import warnings
}

// the void status(std::ostream) methods are useless within Ruby
// convert them to a std::string status() method
%define OSTREAM_TO_STRING(class, method)
%extend class {
  std::string method() {
    std::ostringstream os;
    $self->method(os);
    return os.str();
  }
}
%enddef

%exceptionclass kdb::tools::ToolException;
%extend kdb::tools::ToolException {
  %pythoncode %{
    def __str__(self):
      return self.what()
  %}
}
%include "toolexcept.hpp"

/*
 * pluginspec.hpp
 */
%exception {
  KDB_CATCH(PLUGIN_EXCEPTIONS)
}

// properties
// we can't use %attribute here swig won't generate exception code for
// properties. thus we rename and create them using pure python code below
%rename("_%s") kdb::tools::PluginSpec::getFullName;
%rename("_%s") kdb::tools::PluginSpec::setFullName;
%rename("_%s") kdb::tools::PluginSpec::getName;
%rename("_%s") kdb::tools::PluginSpec::setName;
%rename("_%s") kdb::tools::PluginSpec::getConfig;
%rename("_%s") kdb::tools::PluginSpec::setConfig;
%rename("_%s") kdb::tools::PluginSpec::getRefName;
%rename("_%s") kdb::tools::PluginSpec::setRefName;
%rename("_%s") kdb::tools::PluginSpec::isRefNumber;
%rename("_%s") kdb::tools::PluginSpec::setRefNumber;

%extend kdb::tools::PluginSpec {
  %pythoncode %{
    fullname  = property(_tools.PluginSpec__getFullName, _tools.PluginSpec__setFullName)
    name      = property(_tools.PluginSpec__getName,     _tools.PluginSpec__setName)
    config    = property(_tools.PluginSpec__getConfig,   _tools.PluginSpec__setConfig)
    refName   = property(_tools.PluginSpec__getRefName,  _tools.PluginSpec__setRefName)
    refNumber = property(_tools.PluginSpec__isRefNumber, _tools.PluginSpec__setRefNumber)
  %}
};

%template(PluginSpecVector) std::vector<kdb::tools::PluginSpec>;

%include "pluginspec.hpp"

// clear exception handler
%exception;

/*
 * plugindatabase.hpp
 */
%exception {
  KDB_CATCH(PLUGIN_EXCEPTIONS)
}

%ignore kdb::tools::PluginDatabase::getSymbol;
%ignore kdb::tools::ModulesPluginDatabase::getSymbol;
%ignore kdb::tools::MockPluginDatabase::getSymbol;

%template(VectorStr) std::vector<std::string>;
%shared_ptr(kdb::tools::PluginDatabase);
/* MUST be done for all sub-types too !!! */
%shared_ptr(kdb::tools::ModulesPluginDatabase);
%shared_ptr(kdb::tools::PluginVariantDatabase);
%shared_ptr(kdb::tools::MockPluginDatabase);

%template(IntPluginSpecMap) std::map<int, kdb::tools::PluginSpec>;

%include "plugindatabase.hpp"

// clear exception handler
%exception;

/*
 * plugin.hpp
 */
%exception {
  KDB_CATCH(PLUGIN_EXCEPTIONS)
}

%ignore kdb::tools::Plugin::operator=;
%ignore kdb::tools::Plugin::getSymbol;

%extend kdb::tools::Plugin {
  %pythoncode %{
    fullname  = property(_tools.Plugin_getFullName)
    name      = property(_tools.Plugin_name)
  %}
};

%include "plugin.hpp"

// clear exception handler
%exception;

/*
 * plugins.hpp
 */
%exception {
  KDB_CATCH(PLUGIN_EXCEPTIONS)
}

%include "plugins.hpp"

// clear exception handler
%exception;

/*
 * modules.hpp
 */
%exception {
  KDB_CATCH(PLUGIN_EXCEPTIONS)
}

%newobject kdb::tools::Modules::load;
%extend kdb::tools::Modules {
  Plugin* load(PluginSpec const & spec)
  {
    PluginPtr plugin = $self->load(spec);
    return plugin.release();
  }
};
%ignore kdb::tools::Modules::load;

%include "modules.hpp"

// clear exception handler
%exception;

/*
 * backendparser.hpp
 */
%exception {
  KDB_CATCH(KDB_TOOLS_CATCH_EX(ParseException))
}

%ignore kdb::tools::parseArguments(std::initializer_list<std::string>);
%ignore kdb::tools::detail::processArgument;
%ignore kdb::tools::detail::fixArguments;
%include "backendparser.hpp"

// clear exception handler
%exception;

/*
 * backend.hpp
 */
%exception {
  KDB_CATCH(BACKEND_EXCEPTIONS)
}

%ignore kdb::tools::Backend::operator=;
%ignore kdb::tools::operator<<(std::ostream &, Backend const &);

OSTREAM_TO_STRING(kdb::tools::Backend, status)
OSTREAM_TO_STRING(kdb::tools::ImportExportBackend, status)
%ignore kdb::tools::MountBackendInterface::status;
%ignore kdb::tools::ImportExportBackend::status;
%ignore kdb::tools::Backend::status;

// we cannot wrap std::unique_ptr
%newobject kdb::tools::BackendFactory::create;
%extend kdb::tools::BackendFactory {
  MountBackendInterface* create() const
  {
    MountBackendInterfacePtr backend = $self->create();
    return backend.release();
  }
};
%ignore kdb::tools::BackendFactory::create;

%include "backend.hpp"

// clear exception handler
%exception;

/*
 * backends.hpp
 */
%exception {
  KDB_CATCH(BACKEND_EXCEPTIONS)
}

%template(BackendInfoVector) std::vector<kdb::tools::BackendInfo>;
%immutable kdb::tools::Backends::mountpointsPath;

%include "backends.hpp"

// clear exception handler
%exception;

/*
 * backendbuilder.hpp
 */
%exception {
  KDB_CATCH(BACKEND_EXCEPTIONS)
}

/* this is just a local helper class */
%ignore kdb::tools::BackendBuilderInit;
%immutable kdb::tools::BackendBuilder::globalPluginsPath;

// define traits needed by SwigPyIterator
%fragment("SwigPyIterator_T");
%extend kdb::tools::BackendBuilder {
  swig::SwigPyIterator* __iter__(PyObject **PYTHON_SELF) {
    return swig::make_output_iterator(self->begin(), self->begin(),
      self->end(), *PYTHON_SELF);
  }
}

%include "backendbuilder.hpp"

// clear exception handler
%exception;

/*
 * specreader.hpp
 */
%template(SpecBackendBuilderMap) std::unordered_map<kdb::Key, kdb::tools::SpecBackendBuilder>;

%include "specreader.hpp"

