from os.path import basename, dirname

import uuid
generated_uuid = str(uuid.uuid4()).replace('-','_').upper()

def includeguard(filename):
    if filename == '-':
        return "ELEKTRA_GEN_" + generated_uuid + "_H"
    else:
        return "ELEKTRA_GEN_" + generated_uuid + filename.replace('.','_').upper()

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
