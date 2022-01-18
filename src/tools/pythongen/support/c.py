from support.gen import *
from support.util import *

class CSupport(Support):
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
		return "get_"+self.funcname(key)

	def setfuncname(self, key):
		return "set_"+self.funcname(key)

	def funcpretty(self, key):
		return key.replace('/','_').replace('#','')

	def userkey(self, key):
		"""Return the key name within user:/"""
		if key.startswith('/'):
			return "user"+key
		elif key.startswith('user:/'):
			return key
		elif key.startswith('system:/'):
			return "user"+key[6:]
		else:
			raise Exception("invalid keyname " + key)

	def valof(self, info):
		"""Return the default value for given parameter"""
		val = info["default"]
		type = info["type"]
		if type == "boolean":
			if val == "true":
				return " = 1;"
			elif val == "false":
				return " = 0;"
		elif type == "string" and val == "":
			return ' = "";'
		elif type == "char":
			if val == "' '":
				return " = ' ';"
			else:
				return " = '"+val+"';"
		else:
			return " = "+val+";"

	def typeof(self, info):
		"""Return the type for given parameter"""
		type = info["type"]
		if type == "string":
			return "const char*"
		elif self.isenum(info):
			return "enum " + self.enumname(info)
		else:
			return "kdb_"+type+"_t"

if __name__ == "__main__":
	import doctest
	doctest.testmod()
