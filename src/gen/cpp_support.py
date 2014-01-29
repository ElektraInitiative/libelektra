from c_support import *

#todo: duplicate
def funcname(key):
    if key.startswith('user/'):
        return funcpretty(key[5:])
    elif key.startswith('system/'):
        return funcpretty(key[7:])
    else:
        return funcpretty(key)

def funcpretty(key):
    return key.title().replace('/','').replace('#','')

def valof(info):
    val = info["default"]
    type = info["type"]
    if isenum(info):
        return " = "+enumname(info)+"::"+val+";"
    return " = "+val+";"

def typeof(info):
    type = info["type"]
    if type == "unsigned_int_32":
        return "uint32_t"
    elif type == "string":
        return "std::string"
    elif isenum(info):
        return enumname(info)
    else:
        return type
