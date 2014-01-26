from os.path import basename

def fallback(info):
    return info["fallback"].split(' ')

def override(info):
    return info["override"].split(' ')

def see(info):
    return info["see"].split(' ')

def isenum(info):
    return info["type"].startswith("enum ")

def enumval(info):
    return info["type"].split(' ')[2:]

def enumname(info):
    return info["type"].split(' ')[1]

