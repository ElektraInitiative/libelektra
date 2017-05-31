from support.gen import *
from support.util import *


class ElektraGenSupport(Support):
	def tag_name(self, key):
		if key.startswith('/'):
			return self.tag_pretty(key[1:])
		elif key.startswith('user/'):
			return self.tag_pretty(key[5:])
		elif key.startswith('system/'):
			return self.tag_pretty(key[7:])
		else:
			raise Exception("invalid keyname '" + key + "'")

	def tag_pretty(self, key):
		return "ELEKTRA_TAG_" + key.upper().replace('/','_').replace('#','')

	def upcase_first_letter(self, s):
		return s[:1].upper() + s[1:]

	def tag_type(self, type):
		result = ""
		for part in type.split('_'):
			result += self.upcase_first_letter(part)

		return ("Elektra" + result + "Tag")

	def tag_type_of(self, info):
		"""Return the type for given parameter"""
		type = info["type"]
		if type in ["string", "boolean", "char", "octet", "short", "unsigned_short", "long", "unsigned_long", "long_long", "unsigned_long_long", "float", "double", "long_double"]:
			return self.tag_type(type)
		else:
			raise Exception("invalid type '" + type + "'")

if __name__ == "__main__":
	import doctest
	doctest.testmod()
