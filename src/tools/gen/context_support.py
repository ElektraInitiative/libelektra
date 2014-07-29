from nested_support import *

def nsname(key):
	return nspretty(dirname(dirname((key))))

def funcname(key):
	return funcpretty(basename(key))

def classname(key):
	return classpretty(basename(dirname(key)))

def funcpretty(key):
	"""Return pretty printed key name for functions"""
	return key.title().replace('_','').replace('/','').replace('#','')

def classpretty(key):
	"""Return pretty printed key name for classes"""
	if (key == ''):
		return "Parameters"
	else:
		return key.title().replace('_','').replace('/','').replace('#','N')

def nestedpretty(key):
	"""Getter name for the nested hierarchy"""
	key = key.title().replace('_','').replace('/','').replace('#','N')
	return key[:1].lower() + key[1:]

def nspretty(key):
	"""Return pretty printed key name for namespaces"""
	if key == '/':
		return '' # no namespace
	if key[0] == '/':
		key = key[1:] #cut off / at start
	return key.lower().replace('/','::').replace('#','n')+"::"

def nsnpretty(name):
	"""The namespace name to be used to create a new namespace"""
	return name.lower().replace('#','n')

def below(key, check):
	"""Check if key check is below key"""
	if key == '/':
		return True # everything is below /
	if len(key) > len(check)+1:
		return False # key longer
	if not check.startswith(key):
		return False # not same prefix
	if len(check) == len(key):
		return False # same key
	if check[len(key)] != '/':
		return False # first differ char not /
	return True

def directbelow(key, check):
	"""Check if key check is below key"""
	ckey = check.split('/')
	if key == '/' and len(ckey) == 2:
		return True # all keys with 1 level are below /
	nkey = key.split('/')
	if len(nkey)+1 != len(ckey):
		return False # too many key levels for direct below
	return below(key, check)

def cutname(key, otherkey):
	"""cut the name key to the length of otherkey + 1
	>>> cutname('/hello', '/')
	'/hello'
	>>> cutname('/hello/lift/example', '/hello')
	'/hello/lift'
	>>> cutname('/hello/lift/more/deep', '/hello/lift')
	'/hello/lift/more'
	>>> cutname('/hello/below/nested/deep', '/')
	'/hello'
	"""
	l = 0
	if otherkey == '/':
		l = 2
	else:
		okey = otherkey.split('/')
		l = len(okey)+1
	return '/'.join(key.split('/')[:l])

def sibling(key, check):
	nkey = key.split('/')
	ckey = check.split('/')
	if len(nkey) != len(ckey):
		return False # number of levels must be equal
	pnkey = nkey[:-1]
	pckey = ckey[:-1]
	if pnkey != pckey:
			return False # dirname/parent part must be equal
	return True

def siblings(parameters, key):
	"""Return only keys that are siblings to key"""
	ret = {}
	for k,v in parameters.iteritems():
		if sibling(key, k):
			"sibling", key, k
			ret.update({k: v})
		else:
			"nosibling", key, k
	return ret

def cut(parameters, key):
	"""Return only keys below key
	>>> below('/hello', '/hello/below')
	True
	>>> below('/hello', '/hello/below/deep/below')
	True
	>>> below('/hello', '/notbelow')
	False
	"""
	ret = {}
	for k, v in parameters.iteritems():
		if below(key, k):
			ret.update({k: v})
	return ret

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

def typeof(info):
	"""Return the type for given parameter"""
	if not 'type' in info:
		return "kdb::none_t"
	type = info["type"]
	if type == "string":
		return "std::string"
	elif isenum(info):
		return enumname(info)
	else:
		return "kdb::"+type+"_t"

def quote(s):
	if s[0] != '"' and s[-1] != '"':
		return '"' + s + '"'
	return s

if __name__ == "__main__":
	import doctest
	doctest.testmod()
