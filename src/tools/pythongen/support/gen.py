from os.path import basename, dirname

class Support:
	def trueval(self):
		"""Return all values interpreted as true, everything else is false"""
		return ["true", "1", "on"]

	def min(self, info):
		"""Min range from parameter"""
		return info["range"].split('-')[0]

	def max(self, info):
		"""Max range from parameter"""
		return info["range"].split('-')[1]

	def toarray(self, info, what):
		"""Convert Elektra Array to Python Array"""
		ret = []
		f = info.get(what+"/#0")
		c = 0
		while f is not None:
			ret.append(f)
			c += 1
			f = info.get(what+"/#" + str(c))
		return ret

	def fallback(self, info):
		"""Array of fallback options of parameter"""
		return self.toarray(info, "fallback")

	def override(self, info):
		"""Array of override options of parameter
		>>> info = dict()
		>>> info["override/#0"] = "user:/0"
		>>> info["override/#1"] = "user:/1"
		>>> print info
		{'override/#1': 'user:/1', 'override/#0': 'user:/0'}
		>>> ret = toarray(info, "override")
		>>> print ret
		['user:/0', 'user:/1']
		>>> ret = override(info)
		>>> print ret
		['user:/0', 'user:/1']
		"""
		return self.toarray(info, "override")

	def see(self, info):
		"""Array of "see also" in the parameter"""
		return self.toarray(info, "see")

	def isenum(self, info):
		"""Return if parameter is an enum"""
		return info["type"].startswith("enum ")

	def enumval(self, info):
		"""Return array of all enum values"""
		return info["type"].split(' ')[2:]

	def enumname(self, info):
		"""Return name of enum"""
		return info["type"].split(' ')[1]

	def quote(self, s):
		if len(s) == 0:
			return '""';
		if s[0] != '"' and s[-1] != '"':
			return '"' + s + '"'
		return s

if __name__ == "__main__":
	import doctest
	doctest.testmod()
