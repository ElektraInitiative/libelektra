from os.path import basename, dirname

import uuid
generated_uuid = str(uuid.uuid4()).replace('-','_').upper()

def includeguard(filename):
	if filename == '-':
		return "ELEKTRA_GEN_" + generated_uuid + "_H"
	else:
		return "ELEKTRA_GEN_" + generated_uuid + filename.replace('.','_').upper()

def trueval():
	"""Return all values interpreted as true, everything else is false"""
	return ["true", "1", "on"]

def min(info):
	"""Min range from parameter"""
	return info["range"].split('-')[0]

def max(info):
	"""Max range from parameter"""
	return info["range"].split('-')[1]

def toarray(info, what):
	"""Convert Elektra Array to Python Array"""
	ret = []
	f = info.get(what+"/#0")
	c = 0
	while f is not None:
		ret.append(f)
		c += 1
		f = info.get(what+"/#" + str(c))
	return ret

def fallback(info):
	"""Array of fallback options of parameter"""
	return toarray(info, "fallback")

def override(info):
	"""Array of override options of parameter
	>>> info = dict()
	>>> info["override/#0"] = "user/0"
	>>> info["override/#1"] = "user/1"
	>>> print info
	{'override/#1': 'user/1', 'override/#0': 'user/0'}
	>>> ret = toarray(info, "override")
	>>> print ret
	['user/0', 'user/1']
	>>> ret = override(info)
	>>> print ret
	['user/0', 'user/1']
	"""
	return toarray(info, "override")

def see(info):
	"""Array of "see also" in the parameter"""
	return toarray(info, "see")

def isenum(info):
	"""Return if parameter is an enum"""
	return info["type"].startswith("enum ")

def enumval(info):
	"""Return array of all enum values"""
	return info["type"].split(' ')[2:]

def enumname(info):
	"""Return name of enum"""
	return info["type"].split(' ')[1]

if __name__ == "__main__":
	import doctest
	doctest.testmod()
