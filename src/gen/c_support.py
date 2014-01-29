from gen_support import *

#todo: duplicate
def funcname(key):
    if key.startswith('user/'):
        return funcpretty(key[5:])
    elif key.startswith('system/'):
        return funcpretty(key[7:])
    else:
        return funcpretty(key)

def funcpretty(key):
        return key.replace('/','_').replace('#','')

def valof(info):
    val = info["default"]
    type = info["type"]
    if type == "bool":
        if val == "true":
            return " = 1;"
        elif val == "false":
            return " = 0;"
    else:
        return " = "+val+";"

def typeof(info):
    type = info["type"]
    if type == "unsigned_int_32":
        return "uint32_t"
    elif type == "bool":
        return "int"
    elif type == "string":
        return "char*"
    elif isenum(info):
        return "enum " + enumname(info)
    else:
        return type
