from support.gen import *
from support.util import *

class ElektraGenSupport(Support):
	def tagname(self, key):
		if key.startswith('/'):
			return self.tagpretty(key[1:])
		elif key.startswith('user/'):
			return self.tagpretty(key[5:])
		elif key.startswith('system/'):
			return self.tagpretty(key[7:])
		else:
			raise Exception("invalid keyname '" + key + "'")

	def tagpretty(self, key):
		return "ELEKTRA_TAG_" + key.upper().replace('/','_').replace('#','')

	def tagtypeof(self, info):
		"""Return the type for given parameter"""
		type = info["type"]
		if type == "string":
			return "ElektraStringTag"
		elif type == "boolean":
			return "ElektraBooleanTag"
		elif type == "char":
			return "ElektraCharTag"
		elif type == "octet":
			return "ElektraOctetTag"
		elif type == "short":
			return "ElektraShortTag"
		elif type == "unsigned_short":
			return "ElektraUnsignedShortTag"
		elif type == "long":
			return "ElektraLongTag"
		elif type == "unsigned_long":
			return "ElektraUnsignedLongTag"
		elif type == "long_long":
			return "ElektraLongLongTag"
		elif type == "unsigned_long_long":
			return "ElektraUnsignedLongLongTag"
		elif type == "float":
			return "ElektraFloatTag"
		elif type == "double":
			return "ElektraDoubleTag"
		elif type == "long_double":
			return "ElektraLongDoubleTag"
		else:
			raise Exception("invalid type '" + type + "'")

if __name__ == "__main__":
	import doctest
	doctest.testmod()
