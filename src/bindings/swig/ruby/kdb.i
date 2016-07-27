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


%ignore kdb::Key::Key (const char *keyName, va_list ap);

%include "key.hpp"

%include "kdb.hpp"
