from support.c import *

class CppSupport(CSupport):
	def generateotransform(self, info, index):
		"""Generates the code to transform values"""
		k = "override/#" + str(index) + "/transform/cpp"
		if k in info:
			f = info.get(k)
			return f;
		return "return value"

	def generateftransform(self, info, index):
		"""Generates the code to transform values"""
		k = "fallback/#" + str(index) + "/transform/cpp"
		if k in info:
			f = info.get(k)
			return f;
		return "return value"

	def funcpretty(self, key):
		"""Return pretty printed key name for functions"""
		return key.title().replace('_','').replace('/','').replace('#','')

	def getfuncname(self, key):
		"""CamelCase"""
		return "get"+self.funcname(key)

	def setfuncname(self, key):
		"""CamelCase"""
		return "set"+self.funcname(key)

	def valof(self, info):
		"""Return the default value for given parameter"""
		val = info["default"]
		type = info["type"]
		if self.isenum(info):
			return " = "+self.enumname(info)+"::"+val+";"
		elif type == "string" and val == "":
			return ' = "";'
		return " = "+val+";"

	def typeof(self, info):
		"""Return the type for given parameter"""
		type = info["type"]
		if type == "string":
			return "std::string"
		elif self.isenum(info):
			return self.enumname(info)
		else:
			return "kdb::"+type+"_t"

if __name__ == "__main__":
	import doctest
	doctest.testmod()
