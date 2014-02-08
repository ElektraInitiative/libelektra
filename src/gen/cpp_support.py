from c_support import *

#todo: duplicate
def funcname(key):
    if key.startswith('/'):
        return funcpretty(key[1:])
    elif key.startswith('user/'):
        return funcpretty(key[5:])
    elif key.startswith('system/'):
        return funcpretty(key[7:])
    else:
        raise Exception("invalid keyname " + key)

def funcpretty(key):
    """Return pretty printed key name for functions"""
    return key.title().replace('_','').replace('/','').replace('#','')

def valof(info):
    """Return the default value for given parameter"""
    val = info["default"]
    type = info["type"]
    if isenum(info):
        return " = "+enumname(info)+"::"+val+";"
    elif type == "string" and val == "":
        return ' = "";'
    return " = "+val+";"

def typeof(info):
    """Return the type for given parameter"""
    type = info["type"]
    if type == "unsigned_int_32":
        return "uint32_t"
    elif type == "string":
        return "std::string"
    elif isenum(info):
        return enumname(info)
    else:
        return type
