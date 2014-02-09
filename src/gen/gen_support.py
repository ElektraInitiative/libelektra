from os.path import basename

def trueval():
    """Return all values interpreted as true, everything else is false"""
    return ["true", "1", "on"]

def min(info):
    """Min range from parameter"""
    return info["range"].split('-')[0]

def max(info):
    """Max range from parameter"""
    return info["range"].split('-')[1]

def fallback(info):
    """Array of fallback options of parameter"""
    return info["fallback"].split(' ')

def override(info):
    """Array of override options of parameter"""
    return info["override"].split(' ')

def see(info):
    """Array of "see also" in the parameter"""
    return info["see"].split(' ')

def isenum(info):
    """Return if parameter is an enum"""
    return info["type"].startswith("enum ")

def enumval(info):
    """Return array of all enum values"""
    return info["type"].split(' ')[2:]

def enumname(info):
    """Return name of enum"""
    return info["type"].split(' ')[1]

def below(key, check):
    """Check if key check is below key"""
    if len(key) > len(check)+1:
        return False # key longer
    if not check.startswith(key):
        return False # not same prefix
    if len(check) == len(key):
        return False # same key
    if check[len(key)] != '/':
        return False # first differ char not /
    return True

def cut(parameters, key):
    """Return only keys below key"""
    ret = []
    for parameter in parameters:
        if below(key, parameter[0]):
            ret += parameter
    return ret
