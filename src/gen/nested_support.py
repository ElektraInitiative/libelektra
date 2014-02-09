from cpp_support import *

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
    key = key.title().replace('_','').replace('/','').replace('#','')
    return key[:1].lower() + key[1:]

def nspretty(key):
    """Return pretty printed key name for namespaces"""
    if key[0] == '/':
        key = key[1:]
    return key.lower().replace('/','::').replace('#','N')

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

def nested(key, check):
    nkey = key.split('/')
    ckey = check.split('/')
    if len(nkey) + 2 > len(ckey):
        return None
    pckey = ckey[:len(nkey)]
    if nkey != pckey:
        return None
    return '/'.join(ckey[:len(nkey)+2])

def cutname(key, otherkey):
    """cut the name key to the length of otherkey + 1"""
    okey = otherkey.split('/')
    return '/'.join(key.split('/')[:len(okey)+1])

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

def nestedclasses(parameters, key):
    """Return only keys that are siblings to key"""
    ret = set() # makes sure values are unique
    for k,v in parameters.iteritems():
        if nested(key, k):
            ret.update({nested(key, k)})
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

class Hierarchy:
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
    def info(self):
        return self._info

    @property
    def children(self):
        return self._children

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.name)

    def add(self, hierarchy):
        """Add an element to hierarchy
           >>> h = Hierarchy('/', {'root': 'val', 'key': 'x'})
           >>> h.add(Hierarchy('/hello', {'hello': 'first element'}))
           >>> h.add(Hierarchy('/hello/below', {'k3': 'otherval'}))
           >>> h.add(Hierarchy('/hello/below/nested', {'k4': 'moreotherval'}))
           >>> h.add(Hierarchy('/hello/below/nested/deep', {'k5': 'moreotherval'}))
           >>> h.add(Hierarchy('/hello/below/sibling', {'k6': 'siblingval'}))
           >>> h.add(Hierarchy('/hello/somewhere/else/deep/below', {'k': 'siblingval'}))
           >>> h.add(Hierarchy('/hello/somewhere/else', {'k2': 'k2siblingval'}))
           """
        assert below(self.name, hierarchy.name), ""+self.name+" not below "+ hierarchy.name
        if directbelow(self.name, hierarchy.name):
            """check if child already exists"""
            for c in self._children:
                if hierarchy.name == c.name:
                    """merge info and children into """
                    c._info.update(hierarchy.info)
                    c._children.update(hierarchy.children)
                    return self
            """add new child"""
            return self._children.update({hierarchy})

        for c in self._children:
            if directbelow(c.name, hierarchy.name):
                """add below existing children""", c.name, hierarchy.name
                return c.add(hierarchy)

        """create a hierarchy and add there"""
        h = Hierarchy(cutname(hierarchy.name, self.name), {})
        self._children.update({h})
        return h.add(hierarchy)

    def __str__(self, level = 0):
        ret = ""
        ret += '\t'*level + "/"+basename(self.name) + " " + str(self.info) + "\n"
        for c in self._children:
            ret += c.__str__(level + 1)
        return ret
