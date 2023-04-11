/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%module kdb
#pragma SWIG nowarn=317 // Disable warning: Specialization of non-template

%include <stl.i>
%include "../common.i"
%feature("autodoc", "3");

%inline %{
ssize_t ksSize (const ckdb::_KeySet * ks) {
  return ckdb::ksGetSize (ks);
}

ckdb::_Key * ksAt (ckdb::_KeySet * ks, ssize_t pos) {
  return ckdb::ksAtCursor (ks, pos);
}
%}

/* handle exceptions */
%{
  #define KDB_CATCH_EX(namespace, exception) \
    catch(const namespace::exception &e) \
    { \
      SWIG_Python_Raise(SWIG_NewPointerObj(new namespace::exception(e), \
        SWIGTYPE_p_##namespace##__##exception, SWIG_POINTER_OWN), \
        #exception, SWIGTYPE_p_##namespace##__##exception); \
      SWIG_fail; \
    }
%}

%pythoncode {
  import warnings
  import collections
}

%exceptionclass kdb::Exception;
%extend kdb::Exception {
  %pythoncode %{
    def __str__(self):
      return self.what()
  %}
}
%include "keyexcept.hpp"
%include "kdbexcept.hpp"


/*
 * key.hpp
 */
// exception handling for kdb::Key
%exception {
  KDB_CATCH(KEY_EXCEPTIONS)
}

// constructors
%pythonprepend kdb::Key::Key %{
  value = None
  meta  = {}
  # check for copy constructor
  if len(args) and isinstance(args[0], str):
    arg0, args = args[0], args[1:]

    # add support for kdb.Key(name, value, { meta } )
    if len(args) and isinstance(args[0], (str, bytes)):
      value = args[0]
      args = args[1:]

    flags = 0
    args = iter(args)
    for arg in args:
      if arg == KEY_END:
        break
      elif arg == KEY_SIZE:
        # ignore value
        next(args)
      elif arg == KEY_VALUE:
        value = next(args)
      elif arg == KEY_FUNC:
        #TODO swig directors?
        raise TypeError("Unsupported meta type")
      elif arg == KEY_FLAGS:
        flags = next(args)
      elif arg == KEY_META:
        k = next(args)
        meta[k] = next(args)
      elif isinstance(arg, dict):
        meta.update(arg)
      elif isinstance(arg, int):
        warnings.warn("Deprecated option in keyNew: {0}".format(arg),
          DeprecationWarning)
        flags |= arg
      else:
        warnings.warn("Unknown option in keyNew {0}".format(arg),
          RuntimeWarning)
    args = [ arg0, flags ]
%}

%pythonappend kdb::Key::Key {
  if value:
    self.value = value
  for k in meta:
    self.setMeta(k, meta[k])
}

// properties
// we can't use %attribute here swig won't generate exception code for
// properties. thus we rename and create them using pure python code below
//%attributestring(kdb::Key, std::string, name,     getName, setName);
//%attributestring(kdb::Key, std::string, basename, getBaseName, setBaseName);
%rename("_%s") kdb::Key::getName;
%rename("_%s") kdb::Key::setName;
%rename("_%s") kdb::Key::getBaseName;
%rename("_%s") kdb::Key::setBaseName;

// only accept binary data in binary functions
%typemap(out) std::string kdb::Key::getBinary {
  $result = PyBytes_FromStringAndSize($1.data(), $1.size());
}

%typemap(in) (const void *newBinary, size_t dataSize) {
  Py_ssize_t len;
  if (PyBytes_AsStringAndSize($input, reinterpret_cast<char **>(&$1), &len) == -1)
    return NULL;
  $2 = len;
}

%typemap(out) void *kdb::Key::getValue {
  ssize_t size = arg1->getBinarySize();
  $result = PyBytes_FromStringAndSize((const char*)$1, (size > 0) ? size : 0);
}

// add some other useful methods
%extend kdb::Key {
  Key(const char *name, int flags = 0) {
    return new kdb::Key(name,
      KEY_FLAGS, flags,
      KEY_END);
  }

  int __cmp__(const Key *o) {
    return ckdb::keyCmp($self->getKey(), o->getKey());
  }

  kdb::KeySet *__meta__() {
    return new kdb::KeySet(ckdb::keyMeta($self->getKey()));
  }

  // swig doesnt understand kdb::NameIterator::difference_type
  size_t __len__() {
    return std::distance($self->begin(), $self->end());
  }

  kdb::Key *__copy__() {
    return new kdb::Key($self->dup());
  }

  %pythoncode %{
    def get(self):
      """returns the keys value"""
      if self.isBinary():
        return self._getBinary()
      return self._getString()

    def set(self, value):
      """set the keys value. Can be either string or binary"""
      if isinstance(value, bytes):
        return self._setBinary(value)
      return self._setString(str(value))

    def getMeta(self, name = None):
      """returns a metakey given by name. Name can be either string or Key.
      If no metakey is found None is returned.
      If name is omitted an iterator object is returned.
      """
      if name is not None:
        meta = self._getMeta(name)
        return meta if meta else None
      return self.__metaIter()

    def setMeta(self, name, value):
      """set a new metakey consisting of name and value"""
      if isinstance(value, str):
        return self._setMeta(name, value)
      raise TypeError("Unsupported value type")

    def __metaIter(self):
      metaKeys = self.__meta__()
      size = len(metaKeys)
      for cursor in range(size):
        yield metaKeys._lookup(cursor)


    name     = property(_kdb.Key__getName, _kdb.Key__setName)
    value    = property(get, set, None, "Key value")
    basename = property(_kdb.Key__getBaseName, _kdb.Key__setBaseName)

    def __hash__(self):
      if not self.isNameLocked():
        raise TypeError("Unhashable instance: '%r'. Lock the name first)" % self)
      return hash(self.name)

    def __str__(self):
      return self.name

    def __repr__(self):
      return "kdb.Key(" + repr(self.name) + ")"

    # some helpers
    Array = collections.namedtuple('Array', 'index name basename')
    def array_elements(self):
      basename = self.basename
      if basename[0] != '#':
        raise ValueError("Not an array element")
      x = Key(self.name)
      x.delBaseName()
      return Key.Array(int(basename[1:].strip("_")), x.name, x.basename)
 %}
};

// define traits needed by SwigPyIterator
%fragment("SwigPyIterator_T");
%traits_swigtype(std::string);
%fragment(SWIG_Traits_frag(std::string));
%extend kdb::Key {
  swig::SwigPyIterator* __iter__(PyObject **PYTHON_SELF) {
    return swig::make_output_iterator(self->begin(), self->begin(),
      self->end(), *PYTHON_SELF);
  }

  swig::SwigPyIterator* __reversed__(PyObject **PYTHON_SELF) {
    return swig::make_output_iterator(self->rbegin(), self->rbegin(),
      self->rend(), *PYTHON_SELF);
  }
};

%include "key.hpp"

// metadata
%template(_getMeta) kdb::Key::getMeta<const kdb::Key>;
%template(_setMeta) kdb::Key::setMeta<const char *>;

// clear exception handler
%exception;


/*
 * keyset.hpp
 */
// exception handling for kdb::KeySet
%exception {
  KDB_CATCH(KEY_EXCEPTIONS)
}

%pythonprepend kdb::KeySet::KeySet %{
  orig = []
  if len(args):
    orig = args[1:]
    args = [ args[0] ]
%}

%pythonappend kdb::KeySet::KeySet %{
  for arg in orig:
    if arg is KS_END:
      break
    _kdb.KeySet__append(self, arg)
%}

%rename(__len__) kdb::KeySet::size;

%rename("_%s") kdb::KeySet::lookup;
%rename("_%s") kdb::KeySet::append;
%rename("_lookup") kdb::KeySet::at;

%extend kdb::KeySet {
  KeySet(size_t alloc) {
   return new kdb::KeySet(alloc, KS_END);
  }

  kdb::KeySet *__copy__() {
    return new kdb::KeySet(*$self);
  }

  kdb::KeySet *__deepcopy__(PyObject *memo) {
    (void) PyDict_Check(memo);
    ssize_t size = $self->size();
    kdb::KeySet *ks = new kdb::KeySet(size, KS_END);
    for (elektraCursor cursor = 0; cursor < size; ++cursor)
      ks->append($self->at(cursor)->dup());
    return ks;
  }

  %pythoncode %{
    def lookup(self, name):
      """Lookup a key by name. Name can be either string, Key or indexes.
      If index is negative, search starts at the end.
      Returns None if no key is found.
      """
      key = self._lookup(name)
      return key if key else None

    def append(self, *args):
      """Add a key to the keyset. Argument can be either Key, string or bytes.
      """
      if isinstance(args[0], (str, bytes)):
        args = [ Key(*args) ]
      ret = 0
      for item in args:
        ret = _kdb.KeySet__append(self, item)
      return ret

    def extend(self, list):
      """Extend the keyset by appending all the items from the iterable"""
      return self.append(*list)

    def remove(self, name):
      """Removes a key from the keyset. Argument can be either string or Key.
      It raises a ValueError if there is no such item.
      """
      key = self._lookup(name, KDB_O_POP)
      if not key:
        raise ValueError("ks.remove(x): x not in list")
      return key

    def __getitem__(self, key):
      """See lookup(...) for details.
      Slices and negative indexes are supported as well.
      """
      if isinstance(key, slice):
        return [ self[k] for k in range(*key.indices(len(self))) ]
      elif isinstance(key, ( int )):
        item = self.lookup(key)
        if item is None:
          raise IndexError("index out of range")
        return item
      elif isinstance(key, ( str, Key )):
        item = self.lookup(key)
        if item is None:
          raise KeyError(str(key))
        return item
      raise TypeError("Invalid argument type")

    def __contains__(self, item):
      """See lookup(...) for details"""
      if isinstance(item, ( str, Key )):
        key = self._lookup(item)
        return True if key else False
      raise TypeError("Invalid argument type")

    def __str__(self):
      """print the keyset in array style"""
      return str(list(map(lambda k: str(k), self)))

    def __repr__(self):
      items = list(map(lambda k: repr(k), self))
      return "kdb.KeySet({}, {})".format(len(self), ", ".join(items))

    # some helpers
    def filter(self, func):
      items = list(filter(func, self))
      return KeySet(len(items), *items)

    def filter_below(self, where):
      return self.filter(lambda k: k.isDirectBelow(where))

    def unpack_names(self):
      return set(map(lambda k: k.name, self))

    def unpack_basenames(self):
      return set(map(lambda k: k.basename, self))
  %}
}

// define traits needed by SwigPyIterator
%fragment("SwigPyIterator_T");
%traits_swigtype(kdb::Key);
%fragment(SWIG_Traits_frag(kdb::Key));
%extend kdb::KeySet {
  swig::SwigPyIterator* __iter__(PyObject **PYTHON_SELF) {
    return swig::make_output_iterator(self->begin(), self->begin(),
      self->end(), *PYTHON_SELF);
  }

  swig::SwigPyIterator* __reversed__(PyObject **PYTHON_SELF) {
    return swig::make_output_iterator(self->rbegin(), self->rbegin(),
      self->rend(), *PYTHON_SELF);
  }
}

%include "keyset.hpp"

// clear exception handler
%exception;

/*
 * elektradiff.hpp
 */
// exception handling for kdb::ElektraDiff
%exception {
	KDB_CATCH(ELEKTRADIFF_EXCEPTIONS)
}

%include "elektradiff.hpp"
%include "elektradiffexcept.hpp"

// clear exception handler
%exception;

/*
 * kdb.hpp
 */
// exception handling for kdb::KDB
%exception {
  KDB_CATCH(KDB_EXCEPTIONS)
}

%extend kdb::KDB {
  %pythoncode %{
    def __enter__(self):
      """Internal method for usage with context managers"""
      return self

    def __exit__(self, type, value, tb):
      """Internal method for usage with context managers.
      Closes the database.
      """
      try:
        self.close(Key())
      except:
        pass
  %}
}

%include "std_vector.i"
%include "std_string.i"

namespace std {
  %template(StringVector) vector<string>;
}

%include "kdb.hpp"

// clear exception handler
%exception;
