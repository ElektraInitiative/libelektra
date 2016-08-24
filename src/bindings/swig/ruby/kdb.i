/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

%module kdb_

%include "stl.i"
/* %include "../common.i" */
%feature("autodoc", "3");

%include "attribute.i"
%include "std_string.i"
%include "stdint.i"
%include "exception.i"

%{
  extern "C" {
    #include "kdbconfig.h"
    #include "kdb.h"
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
 * kdb.h
 */
%constant void *KS_END = KS_END;
%constant const char *VERSION = KDB_VERSION;
%constant const short VERSION_MAJOR = KDB_VERSION_MAJOR;
%constant const short VERSION_MINOR = KDB_VERSION_MINOR;
%constant const short VERSION_MICRO = KDB_VERSION_MICRO;
// we only care about the enums. ignore the c functions
%ignore ckdb;
%include "kdb.h"


%ignore kdb::Key::Key (const char *keyName, va_list ap);
%ignore kdb::Key::Key (char const *keyName, ...);
%ignore kdb::Key::Key (ckdb::Key *k);
%ignore kdb::Key::Key (Key &k);
%ignore kdb::Key::Key (Key const &k);

%ignore kdb::Key::operator->;

// predicate methods rename to "is_xxx?" and return Rubys boolean
%predicate kdb::Key::isValid;
%predicate kdb::Key::isSystem;
%predicate kdb::Key::isUser;
%predicate kdb::Key::isString;
%predicate kdb::Key::isBinary;
%predicate kdb::Key::isInactive;
%predicate kdb::Key::isBelow;
%predicate kdb::Key::isBelowOrSame;
%predicate kdb::Key::isDirectBelow;
%predicate kdb::Key::hasMeta;
%predicate kdb::Key::isNull; // TODO: do we need something special here??? 
%predicate kdb::Key::needSync;

%rename("name") kdb::Key::getName;
%rename("name=") kdb::Key::setName;

// autorename and templates has some problems
%rename("set_meta") kdb::Key::setMeta<std::string>;
%rename("get_meta") kdb::Key::getMeta<std::string>;

%alias kdb::Key::setMeta<std::string> "[]="
%alias kdb::Key::getMeta<std::string> "[]"

%include "key.hpp"

// meta data
//%template(getMeta) kdb::Key::getMeta<const kdb::Key>;
%template("set_meta") kdb::Key::setMeta<std::string>;
%template("get_meta") kdb::Key::getMeta<std::string>;


%extend kdb::Key {
  Key(const char *name, int flags = 0) {
    return new kdb::Key(name,
      KEY_FLAGS, flags,
      KEY_END);
  }
}


%include "kdb.hpp"
