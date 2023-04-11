%module errors

%include "../common.i"

%include <stl.i>
%include "../common.i"
%feature("autodoc", "3");

%import "kdb.i"

%pythoncode {
class ElektraError:
    number = ""
    description = ""
    module = ""
    file = ""
    line = ""
    mountpoint = ""
    configfile = ""
    reason = ""


class ElektraWarning(ElektraError):
    pass


def get_value_or_empty_string (ks: kdb.KeySet, key_name: str) -> str:
    if ks.__contains__ (key_name):
        key: kdb.Key = ks[key_name]
        if key.value is not None:
            return key.value
    return ""


def get_warnings(key: kdb.Key) -> [ElektraWarning]:
    result = []

    if key.hasMeta("meta:/warnings") is False:
        return result

    meta: kdb.KeySet = key.__meta__().dup()
    warnings: kdb.KeySet = meta.cut("meta:/warnings")
    if len(warnings) == 0:
        return result

    key: kdb.Key
    for key in warnings:
        if key.isDirectBelow("meta:/warnings"):
            warning = ElektraWarning()
            warning.number = get_value_or_empty_string(warnings, key.name + "/number")
            warning.description = get_value_or_empty_string(warnings, key.name + "/description")
            warning.module = get_value_or_empty_string(warnings, key.name + "/module")
            warning.file = get_value_or_empty_string(warnings, key.name + "/file")
            warning.line = get_value_or_empty_string(warnings, key.name + "/line")
            warning.mountpoint = get_value_or_empty_string(warnings, key.name + "/mountpoint")
            warning.configfile = get_value_or_empty_string(warnings, key.name + "/configfile")
            warning.reason = get_value_or_empty_string(warnings, key.name + "/reason")
            result.append(warning)

    return result


def get_error(key: kdb.Key) -> ElektraError | None:
    if key.hasMeta("meta:/error") is False:
        return None

    error = ElektraError()
    error.number = get_value_or_empty_string(key.__meta__(), "meta:/error/number")
    error.description = get_value_or_empty_string(key.__meta__(), "meta:/error/description")
    error.module = get_value_or_empty_string(key.__meta__(), "meta:/error/module")
    error.file = get_value_or_empty_string(key.__meta__(), "meta:/error/file")
    error.line = get_value_or_empty_string(key.__meta__(), "meta:/error/line")
    error.mountpoint = get_value_or_empty_string(key.__meta__(), "meta:/error/mountpoint")
    error.configfile = get_value_or_empty_string(key.__meta__(), "meta:/error/configfile")
    error.reason = get_value_or_empty_string(key.__meta__(), "meta:/error/reason")

    return error

};
