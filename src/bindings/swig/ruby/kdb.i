/**
 * @file
 *
 * @brief Swig interface file for KDB Ruby bindings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%feature("autodoc", "3");

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
/* docstring for module implemented for swig >= 3.0.18 */
%module(docstring=DOCSTRING) kdb
#pragma SWIG nowarn=317 // Disable warning: Specialization of non-template
#pragma SWIG nowarn=378 // Disable warning: operator!= ignored


%include "attribute.i"
%include "std_string.i"
%include "stdint.i"
%include "exception.i"
%include "std_except.i"

/* add mapping for std::bad_alloc exception */
namespace std {
  %std_exception_map(bad_alloc, SWIG_MemoryError);
}


%{
  extern "C" {
    #include "kdbconfig.h"
    #include <elektra/kdb.h>
  }

  #include "keyexcept.hpp"
  #include "kdbexcept.hpp"
  #include "key.hpp"
  #include "keyset.hpp"
  #include "kdb.hpp"
  using namespace kdb;
%}

%apply long { ssize_t }

/****************************************************************************
 *
 * kdb.h
 *
 ****************************************************************************/

%constant void *KEY_END = KEY_END;
%constant void *KS_END = KS_END;
%constant const char *VERSION = KDB_VERSION;
%constant const short VERSION_MAJOR = KDB_VERSION_MAJOR;
%constant const short VERSION_MINOR = KDB_VERSION_MINOR;
%constant const short VERSION_PATCH = KDB_VERSION_PATCH;
/* we only care about the enums. ignore the c functions */
%ignore ckdb;
%include <elektra/kdb.h>



/****************************************************************************
 *
 * kdb::Key
 *
 ****************************************************************************/

%feature("autodoc", "Wrapper class for C++ kdb::Key, thus for a full
documentation see " CPPDOCURL "Key.html.

However, to allow a more Ruby way of programming this class differs from
the original C++ class in the following aspects:
- method names follow ruby naming conventions
- variable length argument list (`va_list`) is implemented using Rubys
  parameter Hash method.
- getter methods to underlaying Elektra Key (C) are not included
- Key.meta allows access to the meta data key set
- string length methods are not included (e.g. key.getNameSize())
") kdb::Key;

/*
 * Exceptions
 */
%exceptionclass kdb::Exception;
%rename("to_s") kdb::Exception::what;

%exceptionclass kdb::KeyInvalidName;
%exceptionclass kdb::KeyException;
%exceptionclass kdb::KeyNotFoundException;
%exceptionclass kdb::KeyTypeException;
%exceptionclass kdb::KeyTypeConversion;

%include "keyexcept.hpp"

/* define which methods are throwing which exceptions */
%catches (kdb::KeyException) kdb::Key::getName;

%catches (kdb::KeyInvalidName) kdb::Key::setName;
%catches (kdb::KeyInvalidName) kdb::Key::addName;
%catches (kdb::KeyInvalidName) kdb::Key::setBaseName;
%catches (kdb::KeyInvalidName) kdb::Key::addBaseName;

%catches (kdb::KeyTypeMismatch, kdb::KeyException) kdb::Key::getString;
%catches (kdb::KeyTypeMismatch, kdb::KeyException) kdb::Key::getBinary;

%catches (kdb::KeyInvalidName) kdb::Key::Key;


/* ignore certain methods */
%feature("autodoc", "Allocate a new Key

The following variants are available:

  call-seq:
     Kdb::Key.new
     Kdb::Key.new keyname
     Kdb::Key.new keyname, options-Hash

  k = Kdb::Key.new

  k = Kdb::Key.new('user:/myapp/config1',
                   value: 'hello',
                   owner: 'me',
                   meta-data1: 'meta')
") kdb::Key::Key;
//%ignore kdb::Key::Key ();
//%ignore kdb::Key::Key (const std::string keyName, ...);
//%ignore kdb::Key::Key (const char *keyName, va_list ap);
%ignore kdb::Key::Key (char const *keyName, ...);
%ignore kdb::Key::Key (ckdb::Key *k);
%ignore kdb::Key::Key (Key &k);
%ignore kdb::Key::Key (Key const &k);

%ignore kdb::Key::operator++;
%ignore kdb::Key::operator--;
%ignore kdb::Key::operator=;
%ignore kdb::Key::operator->;
%ignore kdb::Key::operator bool;

/* This seems to be implemented in ruby by '! ==' */
%ignore kdb::Key::operator!=;

/* we do not need the raw key */
%ignore kdb::Key::getKey;
%ignore kdb::Key::operator*;
%ignore kdb::Key::release;

/* we do not need the string sizes functions, since the give wrong
 * (size + 1) size info */
%ignore kdb::Key::getNameSize;
%ignore kdb::Key::getBaseNameSize;
%ignore kdb::Key::getStringSize;
/* kdb::Key::getBinarySize could be useful */


/* predicate methods rename to "is_xxx?" and return Rubys boolean */
%predicate kdb::Key::isValid;
%predicate kdb::Key::isSystem;
%predicate kdb::Key::isUser;
%predicate kdb::Key::isCascading;
%predicate kdb::Key::isSpec;
%predicate kdb::Key::isProc;
%predicate kdb::Key::isDir;
%predicate kdb::Key::isString;
%predicate kdb::Key::isBinary;
%predicate kdb::Key::isBelow;
%predicate kdb::Key::isBelowOrSame;
%predicate kdb::Key::isDirectBelow;
%predicate kdb::Key::hasMeta;
%predicate kdb::Key::isNull;
%predicate kdb::Key::needSync;

/*
 * be more Ruby native:
 * for all methods, which return a Key, for which Key.is_null? returns true
 * (null-key), return NIL instead */
namespace kdb {

  %typemap(out) Key {
    if ($1.isNull()) {
      $result = Qnil;
    } else {
      $result = SWIG_NewPointerObj(new kdb::Key($1),
                                    SWIGTYPE_p_kdb__Key,
                                    SWIG_POINTER_OWN | 0);
    }
  }
}


/* rename some methods to meet the Ruby naming conventions */
%rename("name") kdb::Key::getName;
%rename("name=") kdb::Key::setName;

%rename("basename") kdb::Key::getBaseName;
%rename("basename=") kdb::Key::setBaseName;

%rename("add_basename") kdb::Key::addBaseName;

%rename("namespace") kdb::Key::getNamespace;


/* autorename and templates has some problems */
%rename("get") kdb::Key::get<std::string>;
%rename("set") kdb::Key::set<std::string>;
%alias kdb::Key::get<std::string> "value"
%alias kdb::Key::set<std::string> "value="

%rename("set_meta") kdb::Key::setMeta<std::string>;
%rename("get_meta") kdb::Key::getMeta<std::string>;

%alias kdb::Key::setMeta<std::string> "[]="
%alias kdb::Key::getMeta<std::string> "[]"

/* expose Keys meta KeySet
 * this allows Ruby-style meta iterator, e.g. k.meta.each ... */
%feature("autodoc", "allows access to the meta data keyset of the
underlaying key, which allows a Ruby-style iteration over metadata:
  k.meta.each { |m| puts 'meta data: %s: %s' % [m.name, m.value] }
") kdb::Key::meta;
/* key.meta will return a newly created object Ruby should garbage collect */
%newobject kdb::Key::meta;
%extend kdb::Key {
  kdb::KeySet* meta() {
    /* create a new KeySet with all meta keys added */
    ckdb::KeySet* curMetaKeys = ckdb::keyMeta ($self->getKey ());
    kdb::KeySet* metaKeys = new kdb::KeySet();

    ckdb::Key* curMeta;
    ssize_t it = 0;

    while ((curMeta = ckdb::ksAtCursor (curMetaKeys, it++)) != nullptr) {
      kdb::Key keyToAppend (curMeta);
      metaKeys->append(keyToAppend);
    }

    return metaKeys;
  }
}


/* getMeta Typemap
 * This is used to convert the input argument to a Ruby string. In certain
 * cases this is useful, to allow passing in Symbols as meta names. */
%typemap(in) (const std::string & metaName) {
  // typemap in for getMeta
  $input = rb_funcall($input, rb_intern("to_s"), 0, Qnil);
  $1 = new std::string(StringValueCStr($input));
}
%typemap(freearg) (const std::string & metaName) {
  // typemap in for getMeta
  delete $1;
}

/* Typemap for setBinary
 * pass raw data pointer of a Ruby String and its length */
%typemap(in) (const void * newBinary, size_t dataSize) {
  $1 = (void *) StringValuePtr($input);
  $2 = RSTRING_LEN($input);
}


/* 'imitate' va_list as Ruby Hash
 *
 * "misuse" the exception feature of SWIG to provide a custom
 *  method invocation. This allows us to pass a Ruby argument hash
 *  as a va_list. This way, we can imitate the variable argument
 *  list (and keyword argument) features
 */
%typemap(in) (va_list ap) {
  // we expect to be $input to be a Ruby Hash
  Check_Type($input, T_HASH);
}

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


  /* $input substitution does not here, so we have to reference
     input variables directly */

  hash_size = NUM2INT(rb_funcall(argv[1], rb_intern("size"), 0, Qnil));
  keys_arr = rb_funcall(argv[1], rb_intern("keys"), 0, Qnil);
  if (hash_size > 0) {
    /* first we check if we can find the "flags" key.
       this has to be passed to the kdb::Key constructor already */
    for (i = 0; i < hash_size; i++) {
      key = rb_ary_entry(keys_arr, i);
      val = rb_hash_aref(argv[1], key);
      /* convert key to String, in case of being a Symbol */
      key = rb_funcall(key, rb_intern("to_s"), 0, Qnil);
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
     we have to do it ourself (not very portable)
  */
  try {
    result = (kdb::Key *)new kdb::Key((char const *)arg1,
      KEY_FLAGS, flags,
      KEY_END);
  } catch (std::bad_alloc &_e) {
    SWIG_exception_fail(SWIG_MemoryError, (&_e)->what());
  }
  DATA_PTR(self) = result;

  if (hash_size > 0) {
    /* now treat (nearly) all key-value pairs as meta data, thus
       assign it to the newly created kdb::Key object */
    for (i = 0; i < hash_size; i++) {
      key = rb_ary_entry(keys_arr, i);
      val = rb_hash_aref(argv[1], key);
      key = rb_funcall(key, rb_intern("to_s"), 0, Qnil);
      val = rb_funcall(val, rb_intern("to_s"), 0, Qnil);
      /* ignore certain keys */
      if (strcmp("flags", StringValueCStr(key)) == 0) continue;
      /* 'value' has also a special meaning */
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


/* universal 'get' and 'set' (value) methods
 *
 * This allows the univeral use of get/set methods, while really
 * calling get|setBinary|String depending on the current Key
 * type */
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


/*
 * Iterators
 */
#define ELEKTRA_WITHOUT_ITERATOR

/*
 * Key clonging
 */
%ignore kdb::Key::dup;
%ignore kdb::Key::copy;

%alias kdb::Key::clone() "dup"

/* clone definitely creates a new object, Ruby shall take ownership */
%newobject kdb::Key::clone;

%extend kdb::Key {
  kdb::Key *clone() {
    kdb::Key *k;
    k = new kdb::Key();
    k->copy(*$self);
    return k;
  }
}


/*
 * Key callback methods
 * (ignore them for now)
 */
%ignore kdb::Key::setCallback;
%ignore kdb::Key::getFunc;

/*
 * spaceship operator, useful for sorting methods
 */
%feature("autodoc", "<=>(Key comp) -> int

aliased to '<=>', implemented for sorting operations.
  k1 < k2  : -1
  k1 == k2 :  0
  k1 > k2  :  1
") kdb::Key::spaceship;
/* this doesn't work here. "Can't wrap unless renamed to a valid identifier"
 * %rename("<=>") kdb::Key::spaceship; */
%alias kdb::Key::spaceship "<=>"
%extend kdb::Key {
  int spaceship(const kdb::Key &comp) {
    int ret = ckdb::keyCmp ($self->getKey(), comp.getKey());
    if (ret < 0) return -1;
    if (ret > 0) return 1;
    return 0;
  }
}


/*
 * parse key.hpp
 */
%include "key.hpp"


/*
 * used Templates
 */
/* value methods */
%template("get") kdb::Key::get<std::string>;
%template("set") kdb::Key::set<std::string>;

/* meta data */
//%template(getMeta) kdb::Key::getMeta<const kdb::Key>;
%template("set_meta") kdb::Key::setMeta<std::string>;
%template("get_meta") kdb::Key::getMeta<std::string>;




/****************************************************************************
 *
 * kdb::KeySet
 *
 ****************************************************************************/

/* ignore unused constructors */
%ignore kdb::KeySet::KeySet (ckdb::KeySet * k);
%ignore kdb::KeySet::KeySet (size_t alloc, ...);
%ignore kdb::KeySet::KeySet (VaAlloc alloc, va_list ap);
%ignore kdb::KeySet::KeySet (Key, ...);

%ignore kdb::VaAlloc;


/* ignore raw ckdb::KeySet methods */
%ignore kdb::KeySet::getKeySet;

/* ignore unused operators */
%ignore kdb::KeySet::operator=;
/* KeySet == operator see below */
%ignore kdb::operator== (const KeySet &, const KeySet &);
%ignore kdb::operator!= (const KeySet &, const KeySet &);


/*
 * Constructors
 */
/* add a custom constructor for KeySet::KeySet(Key)
 * to enable passing a single Key, or an Array of Keys.
 * This allows KeySet creation in a more Ruby way */
/* first check if we've got a Key or a Ruby-array */
%typemap(in) (kdb::Key*) {
  $1 = NULL;
  if (!RB_TYPE_P($input, T_ARRAY)){
    if (SWIG_ConvertPtr($input, (void**)&$1, SWIGTYPE_p_kdb__Key, 0) == -1) {
      rb_raise(rb_eArgError, "Argument has to be of Type Kdb::Key or Array");
      SWIG_fail;
    }
  }
}
/* define a custom KeySet creation to be able to append the given Key
 * arguments to the newly created KeySet */
%feature("except") kdb::KeySet::KeySet (Key*) {
  if (arg1 != NULL) {
    /* we got a kdb::Key argument (see corresponding typemap)
       so simply use our custom constructor*/
    $action
  } else {
    /* Ruby-Array */
    result = (kdb::KeySet *)new kdb::KeySet();

    /* append each array element, while checking if we really got a Key */
    for (int i = 0; i < RARRAY_LEN(argv[0]); i++) {
      VALUE e;
      kdb::Key *ek = NULL;
      e = rb_ary_entry(argv[0], i);
      if (SWIG_ConvertPtr(e, (void**)&ek, SWIGTYPE_p_kdb__Key, 0) == -1) {
        /* delete the new KeySet first, rb_raise will not return */
        delete result;
        rb_raise(rb_eArgError,
            "Array element at index %d is not of Type Kdb::Key", i);
        SWIG_fail;
      }
      result->append(*ek);
    }
    DATA_PTR(self) = result;
  }
}
/* the custom constructor */
%extend kdb::KeySet {
    KeySet(Key* key) {
        KeySet* ks = new KeySet();
        ks->append(*key);
        return ks;
    }
}


/*
 * Ruby-style iteration
 */
/*
 * KeySet.each
 * Hint: this implementation of 'each' only works wich references to keys
 * so any modifications of the keys are persisted
 */
%extend kdb::KeySet {
  void each() {
    if (rb_block_given_p()) {
      for (ssize_t it = 0; $self->at(it); ++it) {
        VALUE cur;
        Key * t = new Key($self->at(it));
        cur = SWIG_NewPointerObj(t, SWIGTYPE_p_kdb__Key, 1);

        rb_yield(cur);
        /* nothing to free here, Ruby is owner of the new Key obj, which
           will be freed by the garbage collector */
      }
    }
  }
}
/* include Enumerable which adds lots of Ruby iter., search... functions */
%mixin kdb::KeySet "Enumerable";


/*
 * append methods
 */
%alias kdb::KeySet::append "<<"

/* define special typemap for append(KeySet), to allow
 * passing a Ruby-Array also (a little bit hacky) */
%typemap(in) (const kdb::KeySet & toAppend) {
  /* in case we have an array, append each element and return */
  if (RB_TYPE_P($input, T_ARRAY)) {
    int size = RARRAY_LEN($input);
    /* ELEKTRA_LOG_DEBUG("append Array of Keys of len %d", size); private API */
    for ( int i = 0; i < size; ++i) {
      Key* k;
      int reskey = 0;
      reskey = SWIG_ConvertPtr(
          rb_ary_entry($input, i), (void**)&k, SWIGTYPE_p_kdb__Key, 0);
      if (!SWIG_IsOK(reskey)) {
        rb_raise(rb_eArgError,
            "Array element at index %d is not of Type Kdb::Key", i);
        SWIG_fail;
      }
      arg1->append(*k);
    }
    /* return within the typemap. not the best way, but can be considered
       to be an optimization */
    return SWIG_From_long(arg1->size());

  } else {
  /* standard case for KeySet, just convert and check for correct type */
    /* ELEKTRA_LOG_DEBUG("append KeySet"); private API */
    if (!SWIG_IsOK(
          SWIG_ConvertPtr($input, (void**)&$1, SWIGTYPE_p_kdb__KeySet, 0))) {
      rb_raise(rb_eArgError,
          "Argument not of Type Kdb::KeySet");
      SWIG_fail;
    }
  }
}


/*
 * cursor operations
 */
%apply long { elektraCursor }
%alias kdb::KeySet::at "[]"


/*
 * comparision operator
 * this is required, since operator== is not part of KeySet, thus
 * SWIG doesn't add this to class KeySet
 * (otherwise 'kdb::== ks1, ks2' would be required)
 */
%alias kdb::KeySet::operator== "eql?"
%extend kdb::KeySet {
  bool operator== (const KeySet & rhs) {
    return *$self == rhs;
  }
}


/*
 * lookup
 */
%apply int { elektraLookupFlags }

/*
 * delete
 *
 * add 'delete' and 'delete_at' short cut methods
 */
/* 'delete' is a reserved C++ keyword */
%rename("delete") kdb::KeySet::delete_;

%extend kdb::KeySet {
  Key delete_at(elektraCursor pos) {
    Key k = $self->at(pos);
    if (!k.isNull()) {
      $self->lookup(k, KDB_O_POP);
    }
    return k;
  }

  Key delete_(const Key & key) {
    return $self->lookup(key, KDB_O_POP);
  }

  Key delete_(std::string const & name) {
    return $self->lookup(name, KDB_O_POP);
  }
}


/*
 * dup, copy, clone
 * shallow copy KeySet
 */

/* return a kdb::KeySet instead of a ckdb::KeySet */
%typemap(out) ckdb::KeySet* kdb::KeySet::dup {
  $result = SWIG_NewPointerObj(new KeySet($1),
                                SWIGTYPE_p_kdb__KeySet,
                                SWIG_POINTER_OWN | 0);
}

%alias kdb::KeySet::dup "clone"


/*
 * handy helper methods or common aliases
 */
%rename("empty?") kdb::KeySet::empty;
%extend kdb::KeySet {
  bool empty () {
    return $self->size() == 0;
  }
}

%alias kdb::KeySet::size "length"

/*
 * parse keyset.hpp
 */
%include "keyset.hpp"



/****************************************************************************
 *
 * kdb.hpp
 *
 ****************************************************************************/

%include "kdbexcept.hpp"

%exceptionclass kdb::KDBException;

%catches (kdb::KDBException) kdb::KDB::KDB;
%catches (kdb::KDBException) kdb::KDB::open;
%catches (kdb::KDBException) kdb::KDB::get;
%catches (kdb::KDBException) kdb::KDB::set;

%include "std_vector.i"
%include "std_string.i"

namespace std {
  %template(StringVector) vector<string>;
}

%include "kdb.hpp"
