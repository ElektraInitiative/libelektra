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
    key = key.title().replace('_','').replace('/','').replace('#','N')
    return key[:1].lower() + key[1:]

def nspretty(key):
    """Return pretty printed key name for namespaces"""
    if key == '/':
        return '' # no namespace
    if key[0] == '/':
        key = key[1:] #cut off / at start
    return key.lower().replace('/','::').replace('#','N')+"::"

def nsnpretty(name):
    """The namespace name to be used to create a new namespace"""
    return name.lower().replace('#','N')

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
    def classname(self):
        return classpretty(basename(self.name))

    @property
    def dirname(self):
        return dirname(self._name)

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
                cwc.update({c})
        return cwc

    @property
    def childrenWithType(self):
        cwc = set()
        for c in self._children:
            if 'type' in c.info:
                cwc.update({c})
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
                    assert c._info == {}, "info is not empty in" + c.name
                    c._info.update(hierarchy.info)
                    c._children.update(hierarchy.children)
                    "updated " + hierarchy.name + " in child " + c.name
                    return
            """add new child below myself""" + hierarchy.name + " into " + self.name
            self._children.update({hierarchy})
            return

        for c in self._children:
            if below(c.name, hierarchy.name):
                """add below existing children""", c.name, hierarchy.name
                return c.add(hierarchy)

        n = cutname(hierarchy.name, self.name)
        h = self.structure(n)
        """create a structure hierarchy""",hierarchy.name, self.name, h.name
        self._children.update({h})
        h.add(hierarchy)
        return

    def __str__(self, level = 0):
        ret = ""
        ret += '        '*level + "/" + self.basename + " " + str(self.info) + "\n"
        for c in self._children:
            ret += c.__str__(level + 1)
        return ret

if __name__ == "__main__":
    import doctest
    doctest.testmod()
