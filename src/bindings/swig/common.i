/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%define ELEKTRA_SENTINEL %enddef

%include <attribute.i>
%include <std_string.i>
%include <stdint.i>
%include <exception.i>

%{
  extern "C" {
    #include <internal/kdb/config.h>
    #include <elektra/old_kdb.h>
  }

  #include "keyexcept.hpp"
  #include "kdbexcept.hpp"
  #include "key.hpp"
  #include "keyset.hpp"
  #include "kdb.hpp"
  using namespace kdb;
%}

%apply long { ssize_t }

/*
 * kdbconfig.h
 */
%constant const char *DB_SYSTEM = KDB_DB_SYSTEM;
%constant const char *DB_USER = KDB_DB_USER;
%constant const char *DB_HOME = KDB_DB_HOME;
%constant bool DEBUG = DEBUG;


/*
 * kdb.h
 */
// Lua doesn't like the void* type of KEY_END for some reason
%constant unsigned long long KEY_END = 0;
%constant void *KS_END = KS_END;
%constant const char *VERSION = KDB_VERSION;
%constant const short VERSION_MAJOR = KDB_VERSION_MAJOR;
%constant const short VERSION_MINOR = KDB_VERSION_MINOR;
%constant const short VERSION_PATCH = KDB_VERSION_PATCH;
// we only care about the enums. ignore the c functions
%ignore ckdb;


/* handle exceptions */
%{
  #define KEY_EXCEPTIONS \
    KDB_CATCH_EX(kdb, KeyNotFoundException) \
    KDB_CATCH_EX(kdb, KeyTypeMismatch) \
    KDB_CATCH_EX(kdb, KeyTypeConversion) \
    KDB_CATCH_EX(kdb, KeyInvalidName) \
    KDB_CATCH_EX(kdb, KeyException) \
    KDB_CATCH_EX(kdb, Exception)

  #define KDB_EXCEPTIONS \
    KDB_CATCH_EX(kdb, ContractException) \
    KDB_CATCH_EX(kdb, KDBException) \
    KDB_CATCH_EX(kdb, Exception)
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


/*
 * key.hpp
 */
// operator overloading sucks
%ignore kdb::Key::operator->;
%ignore kdb::Key::operator=;
%ignore kdb::Key::operator+=;
%ignore kdb::Key::operator-=;

// constructors
%ignore kdb::Key::Key (Key const &k);
%ignore kdb::Key::Key (const char *keyName, ...);
%ignore kdb::Key::Key (const std::string keyName, ...);
%ignore kdb::Key::Key (const char *keyName, va_list ap);

// ignore functions we also provide char* signatures
%ignore kdb::Key::setString(const std::string&);

// reference handling
%ignore kdb::Key::operator++(int) const;
%ignore kdb::Key::operator--(int) const;
%rename(_incRef) kdb::Key::operator++;
%rename(_decRef) kdb::Key::operator--;

// reference counted object
//%feature("ref")   kdb::Key "$this->operator++();"
//%feature("unref") kdb::Key "$this->operator--();"

// name manipulation
%rename("_%s") kdb::Key::getNameSize;
%rename("_%s") kdb::Key::getBaseNameSize;

// value operations
%rename("_%s") kdb::Key::getString;
%rename("_%s") kdb::Key::setString;
%rename("_%s") kdb::Key::getStringSize;
%rename("_%s") kdb::Key::getFunc;

%rename("_%s") kdb::Key::getBinary;
%rename("_%s") kdb::Key::setBinary;
%rename("_%s") kdb::Key::getBinarySize;
%rename("_%s") kdb::Key::getValue;


/*
 * keyset.hpp
 */
%apply ssize_t { elektraCursor }

%ignore kdb::VaAlloc;
%ignore kdb::KeySet::KeySet (VaAlloc va, va_list ap);
%ignore kdb::KeySet::KeySet (size_t alloc, ...);
%ignore kdb::KeySet::KeySet (Key, ...);
%ignore kdb::KeySet::operator=;


// iterators
// we hide all iterator classes. users should use pairs/ipairs
#define ELEKTRA_WITHOUT_ITERATOR
