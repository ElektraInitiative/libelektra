from ..module import get_introspection_module
from ..overrides import override
import warnings

GElektra = get_introspection_module('GElektra')

def __func_alias(klass, old, new):
	func = getattr(klass, old)
	setattr(klass, new, func)

def __func_rename(klass, old, new):
	__func_alias(klass, old, new)
	delattr(klass, old)

__all__ = []

## make the enums global
for n in GElektra.KeySwitch.__dict__:
	if n.isupper():
		globals()['KEY_' + n] = getattr(GElektra.KeySwitch, n)
		__all__.append('KEY_' + n)

for n in GElektra.KdbOptions.__dict__:
	if n.isupper():
		globals()['KDB_O_' + n] = getattr(GElektra.KdbOptions, n)
		__all__.append('KDB_O_' + n)

KS_END = None
__all__.append('KS_END')

# exceptions
class Exception(Exception):
	def __init__(self, args = "Exception thrown by Elektra"):
		super().__init__(args)
class KeyException(Exception):
	def __init__(self, args = "Exception thrown by a Key, typically "
			"because you called a method on a null key. "
			"Make sure to check this with !key first"):
		super().__init__(args)
class KeyInvalidName(KeyException):
	def __init__(self, args = "Invalid Keyname: keyname needs to start "
			"with user/ or system/"):
		super().__init__(args)
__all__.extend([ 'Exception', 'KeyException', 'KeyInvalidName' ])

## Key
# rename gi-specific functions
__func_rename(GElektra.Key, 'gi_init',      '_init')
__func_rename(GElektra.Key, 'gi_make',      '_make')
__func_rename(GElektra.Key, 'gi_getstring', '_getstring')
__func_rename(GElektra.Key, 'gi_getbinary', '_getbinary')

# Python API convenience
__func_rename(GElektra.Key, 'cmp', '__cmp__')

__func_rename(GElektra.Key, 'setname',         '_setname')
__func_rename(GElektra.Key, 'setbasename',     '_setbasename')
__func_rename(GElektra.Key, 'getnamesize',     '_getnamesize')
__func_rename(GElektra.Key, 'getbasenamesize', '_getbasenamesize')
__func_rename(GElektra.Key, 'getfullnamesize', '_getfullnamesize')

__func_rename(GElektra.Key, 'setstring',    '_setstring')
__func_rename(GElektra.Key, 'setbinary',    '_setbinary')
__func_rename(GElektra.Key, 'getvaluesize', '_getvaluesize')

__func_rename(GElektra.Key, 'rewindmeta',  '_rewindmeta')
__func_rename(GElektra.Key, 'nextmeta',    '_nextmeta')
__func_rename(GElektra.Key, 'currentmeta', '_currentmeta')

class Key(GElektra.Key):
	def __new__(cls, *args):
		# copy constructor
		if len(args) == 1 and isinstance(args[0], cls):
			return super()._make(args[0])
		return super().__new__(cls, args)

	def __init__(self, *args):
		super().__init__()
		if len(args) == 0:
			return

		arg0, *args = args
		# copy constructor has been used, no init needed
		if isinstance(arg0, self.__class__):
			return

		flags = 0
		value = None
		meta  = {}
		args  = iter(args)
		for arg in args:
			if arg == KEY_END:
				break
			elif arg == KEY_SIZE:
				# ignore value
				next(args)
			elif arg == KEY_VALUE:
				value = next(args)
			elif arg == KEY_FUNC:
				raise TypeError("Unsupported meta type")
			elif arg == KEY_FLAGS:
				flags = next(args)
			elif arg == KEY_META:
				k = next(args)
				meta[k] = next(args)
			elif isinstance(arg, GElektra.KeySwitch):
				warnings.warn("Deprecated option in keyNew: {0}".format(arg),
					DeprecationWarning)
				flags |= arg
			else:
				warnings.warn("Unknown option in keyNew: {0}".format(arg),
					RuntimeWarning)

		# _init clears our key
		if isinstance(value, bytes):
			super()._init(arg0, flags | KEY_BINARY, None, value)
		else:
			super()._init(arg0, flags & ~KEY_BINARY, value, None)
		for k in meta:
			self.setmeta(k, meta[k])

	def _setname(self, name):
		ret = super()._setname(name)
		if ret < 0:
			raise KeyInvalidName()
		return ret

	def _setbasename(self, name):
		ret = super()._setbasename(name)
		if ret < 0:
			raise KeyInvalidName()
		return ret

	def addbasename(self, name):
		ret = super().addbasename(name)
		if ret < 0:
			raise KeyInvalidName()
		return ret

	def get(self):
		"""returns the keys value"""
		if self.isbinary():
			return self._getbinary()
		return self._getstring()

	def set(self, value):
		"""set the keys value. Can be either string or binary"""
		if isinstance(value, bytes):
			return self._setbinary(value)
		return self._setstring(str(value))

	def getmeta(self, name = None):
		"""returns a metakey given by name. Name can be either string or Key.
		If no metakey is found None is returned.
		If name is omitted an iterator object is returned.
		"""
		if name is not None:
			meta = super().getmeta(name)
			return meta if meta else None
		return self.__metaIter()

	def setmeta(self, name, value):
		"""set a new metakey consisting of name and value"""
		if isinstance(value, str):
			return super().setmeta(name, value)
		raise TypeError("Unsupported value type")

	def __metaIter(self):
		self._rewindmeta()
		meta = self._nextmeta()
		while meta:
			yield meta
			meta = self._nextmeta()

	def __str__(self):
		return self.name

	def __bool__(self):
		return not self.isnull();

	def __eq__(self, o):
		return self.__cmp__(o) == 0
	def __ne__(self, o):
		return self.__cmp__(o) != 0

	def __gt__(self, o):
		return self.__cmp__(o) > 0
	def __ge__(self, o):
		return self.__cmp__(o) >= 0

	def __lt__(self, o):
		return self.__cmp__(o) < 0
	def __le__(self, o):
		return self.__cmp__(o) <= 0

	name     = property(lambda self: self.get_property('name'), _setname)
	value    = property(get, set, None, "Key value")
	basename = property(lambda self: self.get_property('basename'), _setbasename)
	fullname = property(lambda self: self.get_property('fullname'))

Key = override(Key)
__all__.append('Key')

## KeySet
# rename gi-specific functions
__func_rename(GElektra.KeySet, 'gi_append',        'append')
__func_rename(GElektra.KeySet, 'gi_append_keyset', '_append_keyset')

# Python API convenience
__func_rename(GElektra.KeySet, 'len', '__len__')

__func_rename(GElektra.KeySet, 'lookup_byname', '_lookup_byname')

__func_rename(GElektra.KeySet, 'rewind',   '_rewind')
__func_rename(GElektra.KeySet, 'next',     '_next')
__func_rename(GElektra.KeySet, 'current',  '_current')
__func_rename(GElektra.KeySet, 'atcursor', '_atcursor')

class KeySet(GElektra.KeySet):
	def __new__(cls, *args):
		if len(args) == 1 and isinstance(args[0], __class__):
			return super().dup(args[0])
		return super().__new__(cls, args)

	def __init__(self, *args):
		super().__init__()
		if len(args) == 0:
			return

		arg0, *args = args
		if isinstance(arg0, __class__):
			return

		self.resize(arg0)
		for arg in args:
			if arg is KS_END:
				break
			self.append(arg)

	def lookup(self, name):
		"""Lookup a key by name. Name can be either string, Key or indexes.
		If index is negative, search starts at the end.
		Returns None if no key is found.
		"""
		if isinstance(name, Key):
			key = super().lookup(name, KDB_O_NONE)
		elif isinstance(name, str):
			key = self._lookup_byname(name, KDB_O_NONE)
		elif isinstance(name, int):
			key = self._atcursor(name)
		else:
			raise TypeError("Unsupported type")
		return key if key else None

	def append(self, data):
		if isinstance(data, __class__):
			return self._append_keyset(data)
		return super().append(data)

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
			key = self.lookup(item)
			return True if key else False
		raise TypeError("Invalid argument type")

	def __iter__(self):
		i = 0
		key = self.lookup(i)
		while key:
			yield key
			i = i + 1
			key = self.lookup(i)

KeySet = override(KeySet)
__all__.append('KeySet')

## Kdb
# rename gi-specific functions
__func_rename(GElektra.Kdb, 'gi_open', 'open')

class Kdb(GElektra.Kdb):
	def __init__(self, *args):
		super().__init__()
		self.open(args[0] if len(args) else Key())

	def get(self, ks, parent):
		if isinstance(parent, str):
			parent = Key(parent)
		return super().get(ks, parent)

	def set(self, ks, parent):
		if isinstance(parent, str):
			parent = Key(parent)
		super().set(ks, parent)

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

Kdb = override(Kdb)
KDB = Kdb
__all__.extend([ 'Kdb', 'KDB' ])
