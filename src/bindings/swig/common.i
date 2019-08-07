/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%include "attribute.i"
%include "std_string.i"
%include "stdint.i"
%include "exception.i"

%{
  extern "C" {
    #include "kdbconfig.h"
    #include "elektra/kdb.h"
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
%constant void *KS_END = KS_END;
%constant const char *VERSION = KDB_VERSION;
%constant const short VERSION_MAJOR = KDB_VERSION_MAJOR;
%constant const short VERSION_MINOR = KDB_VERSION_MINOR;
%constant const short VERSION_MICRO = KDB_VERSION_MICRO;
// we only care about the enums. ignore the c functions
%ignore ckdb;
%include "elektra/kdb.h"


/* handle exceptions */
%{
  #define KEY_EXCEPTIONS \
    KDB_CATCH_EX(kdb, KeyTypeMismatch) \
    KDB_CATCH_EX(kdb, KeyInvalidName) \
    KDB_CATCH_EX(kdb, KeyTypeConversion) \
    KDB_CATCH_EX(kdb, KeyException) \
    KDB_CATCH_EX(kdb, Exception)

  #define KDB_EXCEPTIONS \
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
%rename("_%s") kdb::Key::getFullNameSize;

// value operations
%rename("_%s") kdb::Key::getString;
%rename("_%s") kdb::Key::setString;
%rename("_%s") kdb::Key::getStringSize;
%rename("_%s") kdb::Key::getFunc;

%rename("_%s") kdb::Key::getBinary;
%rename("_%s") kdb::Key::setBinary;
%rename("_%s") kdb::Key::getBinarySize;
%rename("_%s") kdb::Key::getValue;

%rename("_%s") kdb::Key::rewindMeta;
%rename("_%s") kdb::Key::nextMeta;
%rename("_%s") kdb::Key::currentMeta;


/*
 * keyset.hpp
 */
%apply ssize_t { cursor_t }

%ignore kdb::VaAlloc;
%ignore kdb::KeySet::KeySet (VaAlloc va, va_list ap);
%ignore kdb::KeySet::KeySet (size_t alloc, ...);
%ignore kdb::KeySet::KeySet (Key, ...);
%ignore kdb::KeySet::operator=;

// iterators
// we hide all iterator classes. users should use pairs/ipairs
#define ELEKTRA_WITHOUT_ITERATOR
