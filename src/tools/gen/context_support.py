from nested_support import *
from kdb_support import *

class ContextSupport(NestedSupport):
	def funcpretty(self, key):
		"""Return pretty printed key name for functions"""
		return key.title().replace('_','').replace('/','').replace('#','')

	def nspretty(self, key):
		"""Return pretty printed key name for namespaces"""
		if key == '/':
			return '' # no namespace
		if key[0] == '/':
			key = key[1:] #cut off / at start
		return key.lower().replace('/','::').replace('#','n')+"::"

	def nsnpretty(self, name):
		"""The namespace name to be used to create a new namespace"""
		return name.lower().replace('#','n')

	def typeof(self, info):
		"""Return the type for given parameter"""
		if not 'type' in info:
			return "kdb::none_t"
		type = info["type"]
		if type == "string":
			return "std::string"
		elif self.isenum(info):
			return self.enumname(info)
		else:
			return "kdb::"+type+"_t"

class ContextHierarchy(Hierarchy):
	def structure(self, n):
		return Hierarchy(n, {"name":n})

	def addWithContext(self, hierarchy):
		capture_id=True
		new_name=""
		for c in hierarchy.name:
			if c == '%':
				capture_id = not capture_id
			elif capture_id:
				new_name += c
		#print "// ADD", hierarchy.name, "new", new_name
		hierarchy.info["name"] = hierarchy.name
		import os.path
		hierarchy._name = os.path.normpath(new_name)
		self.add(hierarchy)
		return

if __name__ == "__main__":
	import doctest
	doctest.testmod()
