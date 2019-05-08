from support.cpp import *
from support.kdb import *

class NestedSupport(CppSupport):
	def nsname(self, key):
		return self.nspretty(dirname(dirname((key))))

	def funcname(self, key):
		return self.funcpretty(basename(key))

	def classname(self, key):
		return self.classpretty(basename(dirname(key)))

	def classpretty(self, key):
		"""Return pretty printed key name for classes"""
		if (key == ''):
			return "Parameters"
		else:
			return key.title().replace('_','').replace('/','').replace('#','N')

	def nestedpretty(self, key):
		"""Getter name for the nested hierarchy"""
		key = key.title().replace('_','').replace('/','').replace('#','N')
		return key[:1].lower() + key[1:]

	def nspretty(self, key):
		"""Return pretty printed key name for namespaces"""
		if key == '/':
			return '' # no namespace
		if key[0] == '/':
			key = key[1:] #cut off / at start
		return key.lower().replace('/','::').replace('#','N')+"::"

	def nsnpretty(self, name):
		"""The namespace name to be used to create a new namespace"""
		return name.lower().replace('#','N')

class Hierarchy:
	""" Build up an hierarchy of keys with info

	>>> h = Hierarchy('/', {'root': 'val', 'key': 'x'})
	>>> h.add(Hierarchy('/hello/below/nested/deep', {'k5': 'moreotherval'}))
	>>> h.add(Hierarchy('/hello/below', {'k3': 'otherval'}))
	>>> h.add(Hierarchy('/hello/below/sibling', {'k6': 'siblingval'}))
	>>> h.add(Hierarchy('/hello/somewhere/else/deep/below', {'k': 'siblingval'}))
	>>> h.add(Hierarchy('/hello/somewhere/else', {'k2': 'k2siblingval'}))
	>>> h.add(Hierarchy('/we/can/add/anywhere', {'k12': 'xkls'}))
	>>> h.add(Hierarchy('/we/can', {'k22': 'ls'}))
	>>> print h
	/ {'root': 'val', 'key': 'x'}
			/we {}
					/can {}
							/add {}
									/anywhere {'k12': 'xkls'}
			/below {'k3': 'otherval'}
					/sibling {'k6': 'siblingval'}
			/can {'k22': 'ls'}
			/hello {}
					/somewhere {}
							/else {'k2': 'k2siblingval'}
									/deep {}
											/below {'k': 'siblingval'}
					/below {}
							/nested {}
									/deep {'k5': 'moreotherval'}
	<BLANKLINE>
	>>> hierarchy=Hierarchy('/', {})
	>>> hierarchy.add(Hierarchy('/test/lift/floor/#3/height',{"x": "y"}))
	>>> hierarchy.add(Hierarchy('/test/lift/floor/number', {"t": "v"}))
	>>> print hierarchy
	/ {}
			/test {}
					/lift {}
							/floor {}
									/#3 {}
											/height {'x': 'y'}
									/number {'t': 'v'}
	<BLANKLINE>
	"""

	def __init__(self, name, info):
		assert isinstance(name, basestring)
		assert isinstance(info, dict)
		self._name = name
		self._info = info
		self._children = set()

	@property
	def name(self):
		return self._name

	@property
	def dirname(self):
		return dirname(self._name)

	def prettyclassname(self, support):
		return support.classpretty(self.basename)

	@property
	def basename(self):
		return basename(self._name)

	@property
	def info(self):
		return self._info

	@property
	def children(self):
		return self._children

	@property
	def childrenWithChildren(self):
		cwc = set()
		for c in self._children:
			if c._children:
				cwc.add(c)
		return cwc

	@property
	def childrenWithType(self):
		cwc = set()
		for c in self._children:
			if 'type' in c.info:
				cwc.add(c)
		return cwc

	def __eq__(self, other):
		if isinstance(other, self.__class__):
			return self.name==other.name
		else:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.name)

	def structure(self, n):
		"""Creates an structure Hierarchy entry"""
		return Hierarchy(n, {})

	def add(self, hierarchy):
		"""Add an element to hierarchy
		   """
		assert below(self.name, hierarchy.name), ""+self.name+" not below "+ hierarchy.name
		if directbelow(self.name, hierarchy.name):
			"""check if child already exists"""
			for c in self._children:
				if hierarchy.name == c.name:
					"""update info and children into previous structure hierarchy"""
					assert len(c._info) == 0, "info is not empty in" + c.name
					c._info.update(hierarchy.info)
					c._children.update(hierarchy.children)
					"updated " + hierarchy.name + " in child " + c.name
					return
			"""add new child below myself""" + hierarchy.name + " into " + self.name
			self._children.add(hierarchy)
			return

		for c in self._children:
			if below(c.name, hierarchy.name):
				"""add below existing children""", c.name, hierarchy.name
				return c.add(hierarchy)

		n = cutname(hierarchy.name, self.name)
		h = self.structure(n)
		"""create a structure hierarchy""",hierarchy.name, self.name, h.name
		self._children.add(h)
		h.add(hierarchy)
		return

	def __str__(self, level = 0):
		ret = ""
		ret += '		'*level + "/" + self.basename + " " + str(self.info) + "\n"
		for c in self._children:
			ret += c.__str__(level + 1)
		return ret

if __name__ == "__main__":
	import doctest
	doctest.testmod()
