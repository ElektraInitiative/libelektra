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
