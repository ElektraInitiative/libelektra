from support.gen import *
from support.util import *

class CSupportElektra(CSupport):
	def funcname(self, key):
		if key.startswith('/'):
			return self.funcpretty(key[1:])
		elif key.startswith('user:/'):
			return self.funcpretty(key[5:])
		elif key.startswith('system:/'):
			return self.funcpretty(key[7:])
		else:
			raise Exception("invalid keyname " + key)

	def getfuncname(self, key):
		return "elektraGet"+self.funcname(key)

	def setfuncname(self, key):
		return "elektraSet"+self.funcname(key)

	def funcpretty(self, key):
		return ''.join(x for x in key.title() if not x == '/' and not x == '_')

if __name__ == "__main__":
	import doctest
	doctest.testmod()
