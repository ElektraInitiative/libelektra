#!/bin/python3

from keynames import Namespace, NAMESPACES_MAP, NAMESPACES, PART_REGEX, ESCAPE_REGEX, ESCAPES, KeyNameException

import re
import sys
import json
from typing import List, Optional, Union, Dict, Tuple, Iterator, Match
from itertools import takewhile

import argparse

NAMESPACES_MAP_INVERSE = {
    Namespace.META: "meta",
    Namespace.SPEC: "spec",
    Namespace.PROC: "proc",
    Namespace.DIR: "dir",
    Namespace.USER: "user",
    Namespace.SYSTEM: "system",
    Namespace.DEFAULT: "default",
}

ESCAPES_FULL = set([".", "..", "%"])

# Demo of new key name API
# reuses keynames.py for convience only.
#
# Changes compared to to keynames.py:
# - '®elektra' is a reserved part, assuming UTF-8 encoding. The UTF-8 encoding of '®' is '\xc2\xae'.
# - part starting with '#' is an array, unless it starts with '##'
# - part starting with '##' is pseudo-array part; it wants to look like an array part but not be an array part; first '#' should be ignored for field names
# - part starting with '%' is not reserved, but '/%/' and '/%' is still empty part in escaped name, can be escaped as '\%'
# - part starting with '_' is not reserved
# - part starting with '@' is not reserved
# - canonicalization of '.' and '..' works like with Unix Paths, but can be escaped as '\.' and '\..'
# - '/' and '\' still have to be escaped in escaped name, the other valid escape sequences are '/\%/', '/\./' and '/\../'
# - unescaping rules are unchanged (apart from a bugfix); parts starting with '##' are NOT unescaped
#
# The CLI also has the new commands 'demo' and 'repl'
#   - 'demo' runs a short demo and a few tests
#   - 'repl' loads this file and drops into a REPL; best way to explore the new API
#


class Key(object):
    # contains relevant key manipulation methods

    namespace: Namespace
    __parts: List[bytes]
    value: Optional[Union[str, int, bool, float]]

    def __init__(self, key: str, value: Optional[Union[str, int, float, bool]] = None):
        self.namespace, parts = Key.unescape(Key.canonicalize(key))
        self.__parts = list(bytes(part, encoding="utf-8") for part in parts)
        if value is None or isinstance(value, bool) or isinstance(value, float) or isinstance(value, int):
            self.value = value
        else:
            self.value = str(value)

    def parts(self) -> List[bytes]:
        return list(self.__parts)

    def name(self) -> str:
        if self.namespace == Namespace.CASCADING:
            prefix = "/"
        else:
            prefix = NAMESPACES_MAP_INVERSE[self.namespace] + ":/"

        return prefix + "/".join(Key.escape_part(part) for part in self.__parts)

    def last_part(self) -> bytes:
        return self.__parts[-1]

    def del_last_part(self):
        del self.__parts[-1]

    @staticmethod
    def base_name(name: bytes) -> bytes:
        if name == bytes("®elektra", encoding="utf-8"):
            raise KeyNameException("'®elektra' is reserved")

        if len(name) > 1 and name[0:1] == b"#":
            return b"#" + name
        else:
            return name

    def add_base_name(self, name: bytes):
        self.__parts.append(Key.base_name(name))

    def set_base_name(self, name: bytes):
        self.del_last_part()
        self.add_base_name(name)

    @staticmethod
    def array_index(index: int) -> bytes:
        str_index = bytes(str(index), encoding="utf-8")
        return b"#" + (b'_' * (len(str_index) - 1)) + str_index

    def add_array_index(self, index: int):
        self.__parts.append(Key.array_index(index))

    def set_array_index(self, index: int):
        self.del_last_part()
        self.add_array_index(index)

    def relative_name(self, parent: 'Key') -> Optional[List[bytes]]:
        parent_len = len(parent.__parts)

        if self.__parts[0:parent_len] != parent.__parts:
            return None

        return self.__parts[parent_len:]

    @staticmethod
    def escape_part(part: bytes) -> str:
        if len(part) == 0:
            return "%"

        return re.sub(f"([\\\\/])", "\\\\\\1", part.decode("utf-8"))

    @staticmethod
    def read_array_index(part: bytes) -> Optional[int]:
        if part[0:1] == b"#":
            underscores = list(takewhile(lambda x: x == b"_",
                                         (part[i:i+1] for i in range(1, len(part)))))
            digits = part[1+len(underscores):]

            if len(underscores) == len(digits) - 1 and re.match(b"[0-9]", digits) is not None:
                return int(digits)

        return None

    @staticmethod
    def canonicalize(name: str, prefix: str = "") -> str:
        if len(name) == 0:
            if len(prefix) == 0:
                raise KeyNameException("Empty name is invalid")

            return prefix

        namespace = None
        namespace_offset = 0

        fullname = None

        if len(prefix) == 0:
            colon_index = name.find(":")
            if colon_index > 0:
                namespace = name[:colon_index]
                namespace_offset = len(namespace)

                if namespace not in NAMESPACES:
                    raise KeyNameException(
                        f"Illegal namespace '{namespace}'. " +
                        f"Allowed namespaces: {NAMESPACES}"
                    )

                fullname = name[colon_index + 1:]
            else:
                fullname = name
        else:
            colon_index = prefix.find(":")
            if colon_index > 0:
                namespace = prefix[:colon_index]
                namespace_offset = len(namespace)

                if namespace not in NAMESPACES:
                    raise KeyNameException(
                        f"Illegal namespace '{namespace}'. " +
                        f"Allowed namespaces: {NAMESPACES}"
                    )

                fullname = prefix[colon_index + 1:] + "/" + name
            else:
                fullname = prefix + "/" + name

        if len(fullname) == 0 or fullname[0] != "/":
            raise KeyNameException(
                f"Key must start with '<NAMESPACE>:/' or just '/'. " +
                f"Allowed values for <NAMESPACE>: {NAMESPACES}"
            )

        if sum(1 for _ in takewhile(lambda x: x == "\\", reversed(fullname))) % 2 != 0:
            raise KeyNameException(
                f"The key must not end with an unescaped backslash '\\'."
            )

        parts = list(PART_REGEX.finditer(fullname))
        part_count = len(parts)
        key_parts: List[Tuple[Match[str], Optional[str]]] = []

        for (index, part) in enumerate(parts):
            full_part = part.group(0)
            actual_part = part.group('content')

            part_start = part.start(0) + namespace_offset
            part_end = part.end(0) + namespace_offset

            if index == part_count - 1:
                if len(actual_part) == 0:
                    break

            # CHANGED: reserved part
            if actual_part == "®elektra":
                raise KeyNameException(
                    f"'®elektra' is a reserved name. " +
                    f"Invalid part '{full_part}' ({part_start}:{part_end})."
                )

            for esc in ESCAPE_REGEX.finditer(actual_part):
                escape = esc.group(1)

                # CHANGED: only escaping full parts ., .., % or / \ anywhere allowed
                if esc.start(0) == 0:
                    if actual_part[1:] not in ESCAPES_FULL and escape not in ESCAPES:
                        raise KeyNameException(
                            f"Illegal escape sequence '{escape}' at start of part. " +
                            f"Invalid part '{full_part}' ({part_start}:{part_end})."
                        )
                elif escape not in ESCAPES:
                    raise KeyNameException(
                        f"Illegal escape sequence '{escape}' (inside part). " +
                        f"Invalid part '{full_part}' ({part_start}:{part_end})."
                    )

            if actual_part == ".":
                # CHANGED: like in Unix Paths .. cannot remove ., instead . is ignored right away
                continue
            elif actual_part == "..":
                if len(key_parts) == 0:
                    # CHANGED: line in Unix Paths, there cannot be to many ..
                    continue

                key_parts.pop()
            elif actual_part[0] == "#" and (len(actual_part) == 1 or actual_part[1] != "#"):
                # CHANGED: parts starting with ## are not invalid array parts, but new pseudo-array parts

                underscores = sum(1 for _ in takewhile(
                    lambda x: x == "_", actual_part[1:]))
                digits = actual_part[underscores + 1:]

                if any(not x.isdigit() for x in digits):
                    raise KeyNameException(
                        f"Array parts (starting with '#') may only contain underscores ('_') and digits ('0'-'9'). " +
                        f"Invalid part: '{full_part}' ({part_start}:{part_end})"
                    )

                if underscores > 0 and underscores != len(digits) - 1:
                    raise KeyNameException(
                        f"Array parts (starting with '#') must consist of either only digits ('0'-'9') or " +
                        f"exactly N underscores ('_') followed by exactly N-1 digits ('0'-'9'). " +
                        f"Invalid part: '{full_part}' ({part_start}:{part_end})"
                    )

                if len(digits) > 19 or (len(digits) == 19 and digits > "9223372036854775807"):
                    raise KeyNameException(
                        f"The maximum array index is 9223372036854775807 (2^64 - 1). " +
                        f"Invalid part: '{full_part}' ({part_start}:{part_end})"
                    )

                if len(digits) > 1 and digits[0] == "0":
                    raise KeyNameException(
                        f"Array indices cannot start with a '0'. " +
                        f"Invalid part: '{full_part}' ({part_start}:{part_end})"
                    )

                key_parts.append((part, f"#{'_' * (len(digits) - 1)}{digits}"))
            else:
                # CHANGED: parts starting with _, @, % have no restrictions
                key_parts.append((part, actual_part))

        key = "/".join(p for (_, p) in key_parts if p is not None)

        if namespace is not None:
            fullkey = f"{namespace}:/{key}"
        else:
            fullkey = f"/{key}"

        return fullkey

    @staticmethod
    def unescape(canonical: str) -> Tuple[Namespace, Iterator[str]]:
        colon_index = canonical.find(":")
        if colon_index > 0:
            namespace = NAMESPACES_MAP[canonical[:colon_index]]
        else:
            namespace = Namespace.CASCADING

        def unescape_part(part: Match[str]) -> str:
            actual_part = part.group("content")

            if actual_part == "%":
                return ""
            else:
                return ESCAPE_REGEX.sub("\\1", actual_part)

        # CHANGED, BUGFIX: ignore empty escaped parts; user:/ produced [b''], but should be []
        unescaped = (
            unescape_part(part) for part in PART_REGEX.finditer(canonical) if len(part.group('content')) > 0
        )

        return (namespace, unescaped)

    def __repr__(self):
        return f"Key({repr(self.name())}, {repr(self.value)})"


class KeyHierarchy(object):
    # New struct to be created in C
    #
    #   struct _KeyHierarchy {
    #       Key * root;      // <- root key of this sub hierarchy, name cascading and relative to parent key
    #       Key * value;     // <- same as Key * as root, if it was present it the original KeySet, NULL otherwise
    #       kdb_long_long_t arraySize;
    #       Key ** array;    // <- list of array children, array[0] is .../#0, etc.
    #       KeySet * map;    // <- non-array children, all keys have single part cascading names relative to root, key values are KeyHierarachy *
    #   };
    #
    #   typedef struct _KeyHierarchy KeyHierarchy;
    #
    #
    # Created by a helper function:
    #   KeyHierarchy * elektraKeyHierarchy (Key * parentKey, KeySet * ks);
    #
    # Additional helper functions for access:
    #   Key * elektraKHRoot (KeyHierarchy * kh);
    #   Key * elektraKHValue (KeyHierarchy * kh);
    #   kdb_long_long_t elektraKHArraySize (KeyHierarchy * kh);
    #   Key ** elektraKHArray (KeyHierarchy * kh);
    #   KeySet * elektraKHMap (KeyHierarchy * kh);
    #
    # And/or traversal function:
    #   typedef void (*elektraKHTraversal) (Key * current, Key * value, kdb_long_long_t arraySize, Key ** array, KeySet * map);
    #   void elektraKHTraverse (KeyHierarchy * kh, elektraKHTraversal traversalFn);
    #
    # Should be used by storage plugins with hierarchical format

    path: List[Union[str, int]]
    value: Optional[Key]
    array_children: List['KeyHierarchy']
    map_children: Dict[str, 'KeyHierarchy']

    @staticmethod
    def create(parent: Key, keys: List[Key]) -> 'KeyHierarchy':
        hierarchy = KeyHierarchy([], None, [], {})

        def convert(part: bytes) -> str:
            if len(part) > 1 and part[0:1] == b"#":
                if part[1:2] == b"#":
                    return part[1:].decode("utf-8")
                else:
                    return Key.read_array_index(part)
            else:
                return part.decode("utf-8")

        for key in keys:
            parts = [convert(part) for part in key.relative_name(parent)]
            KeyHierarchy.__insert(hierarchy, parts, key)

        return hierarchy

    def __init__(self, path: List[Union[str, int]], value: Optional[Key], array_children: List['KeyHierarchy'], map_children: Dict[bytes, 'KeyHierarchy']):
        self.path = list(path)
        self.value = value
        self.array_children = list(array_children)
        self.map_children = dict(map_children)

    def __insert(self, path: List[Union[str, int]], value: Key):
        if len(path) == 0:
            self.value = value
            return

        field = path[0]
        if isinstance(field, str):
            if field not in self.map_children:
                self.map_children[field] = KeyHierarchy(
                    self.path + path, None, [], {})

            self.map_children[field].__insert(path[1:], value)
        elif isinstance(field, int):
            if field < len(self.array_children):
                if self.array_children[field] is None:
                    self.array_children[field] = KeyHierarchy(
                        self.path + path, None, [], {})
            else:
                for _ in range(len(self.array_children), field):
                    self.array_children.append(None)
                self.array_children.append(
                    KeyHierarchy(self.path + path, None, [], {}))

            self.array_children[field].__insert(path[1:], value)
        else:
            raise Exception(f"invalid path part {field}")


class DumpPlugin():
    @staticmethod
    def read_key_name(parent: Key, pkey: str) -> Key:
        return Key(parent.name() + "/" + pkey)

    @staticmethod
    def write_key_name(parent: Key, key: Key) -> str:
        return key.name()[len(parent.name()):]


class PropertiesPlugin():
    # basic plugin using linear list of differently encoded full names

    @staticmethod
    def read_key_name(parent: Key, pkey: str) -> Key:
        name = ""

        escaped = False
        part_start = True
        for c in pkey:
            if escaped:
                escaped = False

                if c == "\\":
                    name += "\\\\"
                elif c in [":", ".", "="]:
                    name += c
                elif c == "n":
                    name += "\n"
                else:
                    raise Exception(f"Illegal escape '\\{c}'")
            elif c == "\\":
                escaped = True
            elif c == "/":
                name += "\\/"
            elif c == ".":
                if part_start:
                    name += "%/"
                else:
                    name += "/"
            else:
                name += c

            part_start = c == "."

        if part_start and len(pkey) > 0:
            name += "%"

        return Key(parent.name() + "/" + name)

    @staticmethod
    def write_key_name(parent: Key, key: Key) -> str:
        return ".".join(re.sub(f"([\\\\.:=])", "\\\\\\1", part.decode("utf-8")).replace("\n", "\\n") for part in key.relative_name(parent))

    @staticmethod
    def read(parent: Key, properties_str: str) -> List[Key]:
        keys = []

        for line in properties_str.split("\n"):
            matches = list(re.finditer(r"(^|[^\\\\])=", line))

            if len(matches) == 0:
                continue

            sep = matches[0]
            name = line[:0 if sep.start(0) == 0 else sep.start(0)+1]
            value = line[sep.end(0):]
            key = PropertiesPlugin.read_key_name(parent, name)
            key.value = value
            keys.append(key)

        return keys

    @staticmethod
    def write(parent: Key, keys: List[Key]) -> str:
        result = ""

        for key in keys:
            result += PropertiesPlugin.write_key_name(parent, key)
            result += "="
            result += str(key.value)
            result += "\n"

        return result


class JsonPlugin():
    # hierachical plugin with arrays, syntax = JSON
    #
    # arrays are encoded as proper JSON arrays
    # the field name "#0" is never an array element
    #
    # holes in arrays need special handling, a missing array element is encoded as { "®elektra": null }
    # The array [ { "®elektra": null }, "b" ] only has a second element, in Elektra that means .../#1=b and .../#0 doesn't exist
    #
    # mixing array and map children also supported
    # The keys .../abc/#0=a .../abc/def=b are encoded as:
    #    "abc": { "®elektra": { "array": [ "a" ] }, "def": "b" }
    #
    # non-leaf keys also need special encoding
    # The keys .../abc=a and .../abc/def=b are encoded as
    #    "abc": { "®elektra": { "value": "a" }, "def": "b" }
    #
    # metadata is not used here, but could similarly be encoded like this
    #    "abc": { "®elektra": { "value": "a", "meta": { "type": "string", "othermeta": 2 } }, "def": "b" }
    #

    # example of direct logic
    @staticmethod
    def key_from_field_path(parent: Key, fields: List[Union[str, int]]) -> Key:
        key = Key(parent.name())

        for field in fields:
            if isinstance(field, int):
                key.add_array_index(field)
            else:
                key.add_base_name(bytes(field, encoding="utf-8"))

        return key

    # example of direct logic
    @staticmethod
    def field_path_from_key(parent: Key, key: Key) -> List[Union[str, int]]:
        def json_field(part: bytes) -> str:
            if len(part) > 1 and part[0:1] == b"#":
                if part[1:2] == b"#":
                    return part[1:].decode("utf-8")
                else:
                    return Key.read_array_index(part)
            else:
                return part.decode("utf-8")

        return list(json_field(part) for part in key.relative_name(parent))

    @staticmethod
    def read(parent: Key, json_str: str) -> List[Key]:
        keys = []
        JsonPlugin.load_json_like(json.loads(json_str), parent, keys, [])
        return keys

    @staticmethod
    def write(parent: Key, keys: List[Key]) -> str:
        return json.dumps(JsonPlugin.create_json_like(parent, keys), indent=2, ensure_ascii=False)

    # helper function
    # in C this should be done directly, by traversing KeyHierarchy and immediately writing to the file
    @staticmethod
    def create_json_like(parent: Key, keys: List[Key]):
        def convert(hierarchy: KeyHierarchy):
            def convert_map():
                return dict((k, convert(v)) for (k, v) in hierarchy.map_children.items())

            def array_data():
                return [{"®elektra": None} if v is None else convert(v) for v in hierarchy.array_children]

            def convert_array():
                return {} if len(hierarchy.array_children) == 0 else {"array": array_data()}

            def convert_value():
                return {} if hierarchy.value.value is None else {"value": hierarchy.value.value}

            if len(hierarchy.map_children) == 0:
                if len(hierarchy.array_children) == 0:
                    if hierarchy.value == None:
                        raise Exception("empty hierarchy")

                    return hierarchy.value.value
                elif hierarchy.value == None:
                    return array_data()

            return {"®elektra": {**convert_value(), **convert_array()}, **convert_map()}

        return convert(KeyHierarchy.create(parent, keys))

    # helper function for Json and Hier
    # in C this should be done directly, by creating Keys while reading to the file
    @staticmethod
    def load_json_like(json_data, parent: Key, keys: List[Key], path: List[Union[str, int]]):
        if isinstance(json_data, list):
            for i, element in enumerate(json_data):
                JsonPlugin.load_json_like(element, parent, keys, path + [i])
        elif isinstance(json_data, dict):
            for k, data in json_data.items():
                if k == "®elektra":
                    if data is None:
                        continue

                    if "value" in data:
                        key = JsonPlugin.key_from_field_path(parent, path)
                        key.value = data["value"]
                        keys.append(key)

                    if "array" in data:
                        for i, element in enumerate(data["array"]):
                            JsonPlugin.load_json_like(
                                element, parent, keys, path + [i]
                            )
                else:
                    JsonPlugin.load_json_like(data, parent, keys, path + [k])
        else:
            key = JsonPlugin.key_from_field_path(parent, path)
            key.value = json_data
            keys.append(key)


class HierPlugin():
    # hierachical plugin without arrays, syntax = JSON minus arrays
    #
    # arrays are encoded likes in Elektra key names
    # the field name "#0" is always an array element, if "##0" is used for 'should be "#0" but not an array element'
    # every key name part is used directly as a JSON field
    #
    # mixed array and map children are supported without special handling, everything is encoded as objects
    # holes in arrays don't need special handling, since arrays encoded as objects
    #
    # non-leaf keys with value are handled like in JsonPlugin
    # metadata would also be handled like in JsonPlugin
    #

    # example of direct logic
    @staticmethod
    def key_from_field_path(parent: Key, fields: List[str]) -> Key:
        key = Key(parent.name())

        for field in fields:
            field_bytes = bytes(field, encoding="utf-8")

            if len(field_bytes) > 1 and field_bytes[0:1] == b"#":
                if field_bytes[1:2] == b"#":
                    key.add_base_name(field_bytes[1:])
                else:
                    key.add_array_index(Key.read_array_index(field_bytes))
            else:
                key.add_base_name(field_bytes)

        return key

    # example of direct logic
    @staticmethod
    def field_path_from_key(parent: Key, key: Key) -> List[Union[str, int]]:
        return list(part.decode("utf-8") for part in key.relative_name(parent))

    @staticmethod
    def read(parent: Key, hier_str: str) -> List[Key]:
        keys = []
        HierPlugin.load_json_like(json.loads(hier_str), parent, keys, [])
        return keys

    @staticmethod
    def write(parent: Key, keys: List[Key]) -> str:
        return json.dumps(HierPlugin.create_json_like(parent, keys), indent=2, ensure_ascii=False)

    # helper function
    # in C this should be done directly, by traversing KeyHierarchy and immediately writing to the file
    @staticmethod
    def create_json_like(parent: Key, keys: List[Key]):
        def convert(hierarchy: KeyHierarchy):
            def convert_map():
                return dict((Key.base_name(bytes(k, encoding="utf-8")).decode("utf-8"), convert(v)) for (k, v) in hierarchy.map_children.items())

            def convert_array():
                return {} if len(hierarchy.array_children) == 0 else dict((Key.array_index(i).decode("utf-8"), convert(v)) for i, v in enumerate(hierarchy.array_children) if (v is not None))

            def convert_value():
                return {} if hierarchy.value.value is None else {"value": hierarchy.value.value}

            if len(hierarchy.map_children) == 0:
                if len(hierarchy.array_children) == 0:
                    if hierarchy.value == None:
                        raise Exception("empty hierarchy")

                    return hierarchy.value.value
                elif hierarchy.value == None:
                    return convert_array()

            return {"®elektra": {**convert_value()}, **convert_array(), **convert_map()}

        return convert(KeyHierarchy.create(parent, keys))

    # helper function for Json and Hier
    # in C this should be done directly, by creating Keys while reading to the file
    @staticmethod
    def load_json_like(json_data, parent: Key, keys: List[Key], path: List[Union[str, int]]):
        if isinstance(json_data, list):
            for i, element in enumerate(json_data):
                HierPlugin.load_json_like(element, parent, keys, path + [i])
        elif isinstance(json_data, dict):
            for k, data in json_data.items():
                if k == "®elektra":
                    if data is None:
                        continue

                    if "value" in data:
                        key = HierPlugin.key_from_field_path(parent, path)
                        key.value = data["value"]
                        keys.append(key)
                else:
                    HierPlugin.load_json_like(data, parent, keys, path + [k])
        else:
            key = HierPlugin.key_from_field_path(parent, path)
            key.value = json_data
            keys.append(key)


class Hier2Plugin():
    # hierachical plugin without arrays, syntax = JSON minus arrays, different encoding of arrays
    #
    # arrays are encoded as { "®elektra": { "array": { "#0": 1, "#1": 2 } } }
    # the field name "#0" is an array element, if and only if it is a child of "array" and that in turn is a child of "®elektra"
    #
    # mixed array and map children are supported without special handling, everything is encoded as objects
    # holes in arrays don't need special handling, since arrays encoded as objects
    #
    # non-leaf keys with value are handled like in JsonPlugin
    # metadata would also be handled like in JsonPlugin
    #

    # example of direct logic
    @staticmethod
    def key_from_field_path(parent: Key, fields: List[str]) -> Key:
        key = Key(parent.name())

        special = False
        array = False
        for field in fields:
            field_bytes = bytes(field, encoding="utf-8")

            if array:
                array = False
                key.add_array_index(Key.read_array_index(field_bytes))
            elif special:
                special = False
                if field == "value":
                    break
                elif field == "array":
                    array = True
            elif field == "®elektra":
                special = True
            else:
                key.add_base_name(field_bytes)

        return key

    # example of direct logic
    @staticmethod
    def field_path_from_key(parent: Key, key: Key) -> List[Union[str, int]]:
        def hier2_field(part: bytes) -> List[str]:
            if len(part) > 1 and part[0:1] == b"#":
                if part[1:2] == b"#":
                    return [part[1:].decode("utf-8")]
                else:
                    return ["®elektra", "array", part.decode("utf-8")]
            else:
                return [part.decode("utf-8")]

        fields = []
        for part in key.relative_name(parent):
            fields.extend(hier2_field(part))
        return fields

    @staticmethod
    def read(parent: Key, hier2_str: str) -> List[Key]:
        keys = []
        Hier2Plugin.load_json_like(json.loads(hier2_str), parent, keys, [])
        return keys

    @staticmethod
    def write(parent: Key, keys: List[Key]) -> str:
        return json.dumps(Hier2Plugin.create_json_like(parent, keys), indent=2, ensure_ascii=False)

    # helper function
    # in C this should be done directly, by traversing KeyHierarchy and immediately writing to the file
    @staticmethod
    def create_json_like(parent: Key, keys: List[Key]):
        def convert(hierarchy: KeyHierarchy):
            def convert_map():
                return dict((k, convert(v)) for (k, v) in hierarchy.map_children.items())

            def array_data():
                return dict((Key.array_index(i).decode("utf-8"), convert(v)) for i, v in enumerate(hierarchy.array_children) if (v is not None))

            def convert_array():
                return {} if len(hierarchy.array_children) == 0 else {"array": array_data()}

            def convert_value():
                return {} if hierarchy.value.value is None else {"value": hierarchy.value.value}

            if len(hierarchy.map_children) == 0:
                if len(hierarchy.array_children) == 0:
                    if hierarchy.value == None:
                        raise Exception("empty hierarchy")

                    return hierarchy.value.value

            return {"®elektra": {**convert_value(), **convert_array()}, **convert_map()}

        return convert(KeyHierarchy.create(parent, keys))

    # helper function for Json and Hier
    # in C this should be done directly, by creating Keys while reading to the file
    @staticmethod
    def load_json_like(json_data, parent: Key, keys: List[Key], path: List[Union[str, int]]):
        if isinstance(json_data, list):
            for i, element in enumerate(json_data):
                Hier2Plugin.load_json_like(element, parent, keys, path + [i])
        elif isinstance(json_data, dict):
            for k, data in json_data.items():
                if k == "®elektra":
                    if data is None:
                        continue

                    if "value" in data:
                        key = Hier2Plugin.key_from_field_path(parent, path)
                        key.value = data["value"]
                        keys.append(key)

                    if "array" in data:
                        for i, element in data["array"].items():
                            Hier2Plugin.load_json_like(
                                element, parent, keys, path + [k, "array", i]
                            )
                else:
                    Hier2Plugin.load_json_like(data, parent, keys, path + [k])
        else:
            key = Hier2Plugin.key_from_field_path(parent, path)
            key.value = json_data
            keys.append(key)


def demo_test():
    print("Constructing key: ")

    print(" - start with 'user:/test/abc'")
    key = Key("user:/test/abc")

    print(" - add base name 'xyz'")
    key.add_base_name(b"xyz")

    print(" - delete last part")
    key.del_last_part()

    print(" - add base name 'd.ef'")
    key.add_base_name(b"d.ef")

    print(" - add base name '#0'")
    key.add_base_name(b"#0")

    print(" - add base name 'xyz'")
    key.add_base_name(b"xyz")

    print(" - set to base name 'ghi'")
    key.set_base_name(b"ghi")

    print(" - add array index 1")
    key.add_array_index(1)

    print(" - add base name 'xyz'")
    key.add_base_name(b"xyz")

    print(" - set to array index 10")
    key.set_array_index(10)

    print(" - add base name '' (empty)")
    key.add_base_name(b"")

    print(" - add base name 'j/k\\l' (slash, backslash)")
    key.add_base_name(b"j/k\\l")

    print(" - add base name '#' (hash)")
    key.add_base_name(b"#")

    print()

    print("Resulting key:  ", "namespace=",
          key.namespace, ", parts=", key.parts(), sep="")
    print("Resulting name: ", key.name(), sep="")
    print()

    try:
        key.add_base_name(b"\xc2\xaeelektra")
    except KeyNameException:
        print("'®elektra' is reserved")
    else:
        print("'®elektra' is not reserved")
    print()

    print("Convert the key 'user:/test/abc/d.ef/##0/ghi/#1/#_10/%/j\\/k\\\\l/#' with parent 'user:/test/abc'")

    parent = Key("user:/test/abc")
    print(
        "- dump:",
        "\n  <-", DumpPlugin.write_key_name(parent, key),
        "\n  ->", DumpPlugin.read_key_name(
            parent,
            "d.ef/##0/ghi/#1/#_10/%/j\\/k\\\\l/#"
        ).name()
    )
    print(
        "- properties:",
        "\n  <-", PropertiesPlugin.write_key_name(parent, key),
        "\n  ->", PropertiesPlugin.read_key_name(
            parent,
            "d\\.ef.##0.ghi.#1.#_10..j/k\\\\l.#"
        ).name()
    )
    print(
        "- json:",
        "\n  <-", JsonPlugin.field_path_from_key(parent, key),
        "\n  ->", JsonPlugin.key_from_field_path(
            parent,
            ['d.ef', '#0', 'ghi', 1, 10, '', 'j/k\\l', '#']
        ).name()
    )

    # hier == "json without arrays"
    print(
        "- hier (json w/o arrays):",
        "\n  <-", HierPlugin.field_path_from_key(parent, key),
        "\n  ->", HierPlugin.key_from_field_path(
            parent,
            ['d.ef', '##0', 'ghi', '#1', '#_10', '', 'j/k\\l', '#']
        ).name()
    )
    print()

    # hier2 == "json without arrays", different array encoding
    print(
        "- hier2 (json w/o arrays, different):",
        "\n  <-", Hier2Plugin.field_path_from_key(parent, key),
        "\n  ->", Hier2Plugin.key_from_field_path(
            parent,
            ['d.ef', '#0', 'ghi', '®elektra', 'array', '#1',
                '®elektra', 'array', '#_10', '', 'j/k\\l', '#']
        ).name()
    )
    print()

    print("Convert keys with parent 'user:/test/abc'")
    print("Keys")
    keys = [
        Key("user:/test/abc", 1),
        Key("user:/test/abc/d.ef", 2),
        Key("user:/test/abc/d.ef/##0", 3),
        Key("user:/test/abc/d.ef/##0/ghi", 4),
        Key("user:/test/abc/d.ef/##0/ghi/#0", 5),
        Key("user:/test/abc/d.ef/##0/ghi/#1", 6),
        Key("user:/test/abc/d.ef/##0/ghi/#1/#_10", 7),
        Key("user:/test/abc/d.ef/##0/ghi/#1/#_10/%", 8),
        Key("user:/test/abc/d.ef/##0/ghi/#1/#_10/%/j\\/k\\\\l", 9),
        Key("user:/test/abc/d.ef/##0/ghi/#1/#_10/%/j\\/k\\\\l/#", 10),
        Key("user:/test/abc/d.ef/##0/ghi/#2", 11),
        Key("user:/test/abc/d.ef/##0/xyz", 12),
        Key("user:/test/abc/d.ef/##0/xyz/#0", 13),
        Key("user:/test/abc/d.ef/##0/xyz/#1", 14),
        Key("user:/test/abc/d.ef/##0/xyz/#1/#0", 15),
        Key("user:/test/abc/d.ef/##0/xyz/#1/#1", 16),
        Key("user:/test/abc/d.ef/##0/xyz/#2/a", 18),
        Key("user:/test/abc/d.ef/##0/xyz/#2", 17),
        Key("user:/test/abc/str=ing", "str"),
        Key("user:/test/abc/nu:ll", None),
    ]
    print("[", *(k for k in keys), sep="\n  ")
    print("]")
    print()

    properties_data = PropertiesPlugin.write(parent, keys)
    print("Properties")
    print(properties_data)

    json_data = JsonPlugin.write(parent, keys)
    print("JSON")
    print(json_data)
    print()

    hier_data = HierPlugin.write(parent, keys)
    print("Hier")
    print(hier_data)
    print()

    hier2_data = Hier2Plugin.write(parent, keys)
    print("Hier2")
    print(hier2_data)
    print()

    print("Reading data back")

    def find(key: Key) -> bool:
        return any((k.name() == key.name() and str(k.value) == str(key.value)) for k in keys)

    properties_keys = PropertiesPlugin.read(parent, properties_data)
    print("Properties", "matches" if len(keys) == len(properties_keys)
          and all(find(k) for k in properties_keys) else "doesn't match")

    json_keys = JsonPlugin.read(parent, json_data)
    print("Json", "matches" if len(keys) == len(json_keys) and all(find(k)
                                                                   for k in json_keys) else "doesn't match")

    hier_keys = HierPlugin.read(parent, hier_data)
    print("Hier", "matches" if len(keys) == len(hier_keys) and all(find(k)
                                                                   for k in hier_keys) else "doesn't match")

    hier2_keys = Hier2Plugin.read(parent, hier2_data)
    print("Hier2", "matches" if len(keys) == len(hier2_keys) and all(find(k)
                                                                     for k in hier2_keys) else "doesn't match")


"""
START OF MAIN FUNCTION
"""

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Check an convert Elektra Key Names."
    )

    subparsers = parser.add_subparsers(
        title="commands",
        dest="command"
    )

    p_canonicalize = subparsers.add_parser(
        "canonicalize",
        description="canonicalize Key Name and print result"
    )
    p_canonicalize.add_argument(
        "--prefix", "-p",
        nargs="?",
        default="",
        help="existing Key Name to which 'name' should be added"
    )
    p_canonicalize.add_argument(
        "name",
        help="the Key Name to canonicalize"
    )

    p_unescape = subparsers.add_parser(
        "unescape",
        description="unescape Key Name and print one part per line"
    )
    p_unescape.add_argument(
        "--raw", "-r",
        action="store_true",
        help="print the raw unescaped Key Name instead of one part per line"
    )
    p_unescape.add_argument(
        "--python-bytes", "-b",
        action="store_true",
        help="print the raw unescaped Key Name as a python bytes object"
    )
    p_unescape.add_argument(
        "name",
        help="the Key Name to unescape"
    )

    p_unescape = subparsers.add_parser(
        "demo",
        description="run demo tests"
    )

    # CHANGED: added repl
    p_unescape = subparsers.add_parser(
        "repl",
        description="open repl"
    )

    args = parser.parse_args()

    if args.command == "demo":
        demo_test()
    elif args.command == "repl":
        import code
        code.InteractiveConsole(locals=globals()).interact()
    elif args.command == "canonicalize":
        try:
            if "prefix" in args:
                key_canonical = Key.canonicalize(args.name, args.prefix)
            else:
                key_canonical = Key.canonicalize(args.name)

            print(key_canonical)
        except KeyNameException as e:
            print("ERROR:", e, file=sys.stderr)
            exit(1)
    elif args.command == "unescape":
        try:
            if "prefix" in args:
                key_canonical = Key.canonicalize(args.name, args.prefix)
            else:
                key_canonical = Key.canonicalize(args.name)

            if args.verbose:
                print(f"[DEBUG] canonical version of name: {key_canonical}")

            (namespace, unescaped) = Key.unescape(key_canonical)

            if args.raw:
                sys.stdout.buffer.write(
                    bytes(namespace.value) +
                    b"\0" +
                    b"\0".join(bytes(p, encoding="utf-8") for p in unescaped) +
                    b"\0"
                )
            elif args.python_bytes:
                print(
                    bytes(namespace.value) +
                    b"\0" +
                    b"\0".join(bytes(p, encoding="utf-8") for p in unescaped) +
                    b"\0"
                )
            else:
                print(str(namespace))
                for part in unescaped:
                    print(part)
        except KeyNameException as e:
            print("ERROR:", e, file=sys.stderr)
            exit(1)
    else:
        parser.print_usage()
