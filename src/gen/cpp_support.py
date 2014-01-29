from c_support import *

def funcname(key):
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
