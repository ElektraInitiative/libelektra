/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

%module kdb

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

/*
 * kdb::Key
 */

//%ignore kdb::Key::Key ();
//%ignore kdb::Key::Key (const std::string keyName, ...);
//%ignore kdb::Key::Key (const char *keyName, va_list ap);
%ignore kdb::Key::Key (char const *keyName, ...);
%ignore kdb::Key::Key (ckdb::Key *k);
%ignore kdb::Key::Key (Key &k);
%ignore kdb::Key::Key (Key const &k);

%ignore kdb::Key::operator->;
%ignore kdb::Key::operator bool;

// we do not need the raw key
%ignore kdb::Key::getKey;
%ignore kdb::Key::operator*;

// we do not need the string sizes functions, since the give wrong
// (size + 1) size info
%ignore kdb::Key::getNameSize;
%ignore kdb::Key::getBaseNameSize;
%ignore kdb::Key::getFullNameSize;
%ignore kdb::Key::getStringSize;


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
%rename("get") kdb::Key::get<std::string>;
%rename("set") kdb::Key::set<std::string>;
%alias kdb::Key::get<std::string> "value"
%alias kdb::Key::set<std::string> "value="

%rename("set_meta") kdb::Key::setMeta<std::string>;
%rename("get_meta") kdb::Key::getMeta<std::string>;

%alias kdb::Key::setMeta<std::string> "[]="
%alias kdb::Key::getMeta<std::string> "[]"

%typemap(in) (const std::string & metaName) {
  // typemap in for getMeta
  $input = rb_funcall($input, rb_intern("to_s"), 0, NULL);
  $1 = new std::string(StringValueCStr($input));
}
%typemap(freearg) (const std::string & metaName) {
  // typemap in for getMeta
  delete $1;
}

%typemap(in) (const void * newBinary, size_t dataSize) {
  $1 = (void *) StringValuePtr($input);
  $2 = RSTRING_LEN($input);
}

%typemap(in) (va_list ap) {
  // we expect to be $input to be a Ruby Hash
  Check_Type($input, T_HASH);
}

/* "missuse" the exception feature of SWIG to provide a custom
   method invocation. This allows us to pass a Ruby argument hash
   as a va_list. This way, we can imitate the variable argument
   list (and keyword argument) features.
*/
%feature("except") kdb::Key::Key (const char *keyName, va_list ap) {
  /* standard method invocation would be: 
  $action
  */
  /* exception features do not have local variables,
     so we define them our selfs */
  int hash_size = 0;
  VALUE keys_arr;
  VALUE key;
  VALUE val;
  int i;
  int flags = 0;

  /* $input substitution does not here, so we have to reverence
     input variables directly */

  hash_size = NUM2INT(rb_funcall(argv[1], rb_intern("size"), 0, NULL));
  keys_arr = rb_funcall(argv[1], rb_intern("keys"), 0, NULL);
  if (hash_size > 0) {
    /* first we check if we can find the "flags" key.
       this has to be passed to the kdb::Key constructor already */
    for (i = 0; i < hash_size; i++) {
      key = rb_ary_entry(keys_arr, i);
      val = rb_hash_aref(argv[1], key);
      /* convert key to String, in case of being a Symbol */
      key = rb_funcall(key, rb_intern("to_s"), 0, NULL);
      /* check for flags and extract them */
      if (strcmp("flags", StringValueCStr(key)) == 0) {
        Check_Type(val, T_FIXNUM);
        flags = NUM2INT(val);
        //printf("got flags: %d\n", flags);
      }
    }
  }
  /* invoke method
     since we can't use arg2 here (is of type va_list)
     we have to do it ourself (new very portable)
  */
  result = (kdb::Key *)new kdb::Key((char const *)arg1, 
      KEY_FLAGS, flags,
      KEY_END);
  DATA_PTR(self) = result;
  
  if (hash_size > 0) {
    /* now treat (nearly) all key-value pairs as meta data, thus
       assign it to the newly created kdb::Key object */
    for (i = 0; i < hash_size; i++) {
      key = rb_ary_entry(keys_arr, i);
      val = rb_hash_aref(argv[1], key);
      key = rb_funcall(key, rb_intern("to_s"), 0, NULL);
      val = rb_funcall(val, rb_intern("to_s"), 0, NULL);
      /* ignore certain keys */
      if (strcmp("flags", StringValueCStr(key)) == 0) continue;
      /* 'value' has also a special meening */
      if (strcmp("value", StringValueCStr(key)) == 0) {
        if (flags & KEY_BINARY) {
          result->setBinary(StringValuePtr(val), RSTRING_LEN(val));
        } else {
          result->setString(StringValueCStr(val));
        }
      } else {
        result->setMeta(StringValueCStr(key), StringValueCStr(val));
      }
    }
  }
  
}


%feature("except") kdb::Key::get<std::string> {
  // redefine our Key::get 
  /*
  $action
  */
  if (((kdb::Key const *)arg1)->isBinary()) {
    result = ((kdb::Key const *)arg1)->getBinary();
  } else {
    result = ((kdb::Key const *)arg1)->getString();
  }
}

%feature("except") kdb::Key::set<std::string> {
  // redefine our Key::set 
  /*
  $action
  */
  if (((kdb::Key const *)arg1)->isBinary()) {
    arg1->setBinary(StringValuePtr(argv[0]),
                RSTRING_LEN(argv[0]));
  } else {
    arg1->setString(StringValueCStr(argv[0]));
  }
}


// Iterators
// exclude them for now
#define ELEKTRA_WITHOUT_ITERATOR


 
%include "key.hpp"

// value methods
%template("get") kdb::Key::get<std::string>;
%template("set") kdb::Key::set<std::string>;

// meta data
//%template(getMeta) kdb::Key::getMeta<const kdb::Key>;
%template("set_meta") kdb::Key::setMeta<std::string>;
%template("get_meta") kdb::Key::getMeta<std::string>;

%include "kdb.hpp"
