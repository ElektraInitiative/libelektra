#!/usr/bin/env python3

"""
This file contains a reference implementation of Elektra's key name processing.

It contains commented functions for:
  - Checking, whether a string is a valid Key Name and if so turning it into its canonical form.
  - Splitting (canonical) Key Names into a Namespace and a list of (unescaped) key name parts.

In addition, we provide a command line interface to experiment with the implementation.
Execute the file with the argument '--help' to find out more about the CLI.
"""

import re
import sys

from itertools import takewhile

from typing import List, Tuple, Match, Optional, Iterator
from enum import Enum

import argparse


def check_array_part(part: str) -> Tuple[bool, Optional[str]]:
    """
    Checks whether `part` is a valid array part.

    If so, the tuple `(True, digits)` is returned where `digits` is a string
    containing the digits part of the Array Part, i.e. the part after the `#` and any `_`.
    Otherwise the tuple `(False, None)` is returned.
    """
    underscores = sum(
        1 for _ in takewhile(lambda x: x == "_", part[1:])
    )
    digits = part[underscores + 1:]

    if any(not x.isdigit() for x in digits):
        return False, None

    if underscores > 0 and underscores != len(digits) - 1:
        return False, None

    if len(digits) > 19 or (len(digits) == 19 and digits > str(2**63-1)):
        return False, None

    if len(digits) > 1 and digits[0] == "0":
        return False, None

    return True, digits


class Namespace(Enum):
    """
    Enum for Elektra's namespaces. Values are used as the first byte in unescaped names.
    """
    CASCADING = 1
    META = 2
    SPEC = 3
    PROC = 4
    DIR = 5
    USER = 6
    SYSTEM = 7
    DEFAULT = 8


# String names for the namespaces
NAMESPACES = {"meta", "spec", "proc", "dir", "user", "system", "default"}

# Map between string namespaces and enum values
NAMESPACES_MAP = {
    "meta": Namespace.META,
    "spec": Namespace.SPEC,
    "proc": Namespace.PROC,
    "dir": Namespace.DIR,
    "user": Namespace.USER,
    "system": Namespace.SYSTEM,
    "default": Namespace.DEFAULT,
}

# Characters that can be escaped anywhere in a Key Name Part
ESCAPES = {"/", "\\"}

# Characters that can be escaped under special conditions
# The value in the dictionary is a function that will be called
# with the Key Name Part that contained the escape sequence.
# The function should return True, iff the escape sequence is valid.
ESCAPES_SPECIAL = {
    ".": lambda part: re.match(r"^\\\.{1,2}$", part),
    "#": lambda part: (len(part) < 21 or part[2:] <= str(2**63-1)) and re.match(r"^\\#[1-9][0-9]{1,18}$", part),
    "%": lambda part: re.match(r"^\\%$", part),
}


# This RegEx defines what a Key Name Part can be.
# A string must match this RegEx to be a Key Name Part, but not all matching strings necessarily are valid Key Name Parts.
#
# The RegEx looks for a leading slash `/`, optionally followed by more slashes `/`.
# All of these slashes will be treated as a single separator.
# After that comes the actual content of the Key Name Part, a series of
#   - characters other than slash `/` and backslash `\`
#   - OR backslash `\` followed by any other character (an escape sequence).
PART_REGEX = re.compile(r"/(?P<leadingSlashes>/*)(?P<content>(?:[^\\/]|\\.)*)")

# This RegEx is used to find escape sequences within a Key Name Part.
ESCAPE_REGEX = re.compile(r"\\(.)")


class KeyNameException(Exception):
    pass


def canonicalize(name: str, prefix: str = "", verbose: bool = False) -> str:
    """
    Returns the canonical version of the Key Name `name`.
    If `name` is not a valid Key Name, a `KeyNameException` will be raised.

    If `prefix` is not empty, `name` is canonicalized under the assumption that it is being appended to the existing Key Name `prefix`.
    It is similar to (but not necessarily the same as) calling `canonicalize (name + "/" + prefix)`.

    If `verbose` is set to `True`, additional information about the procedure is printed to `sys.stderr`.
    """

    # First check for an empty Key Name
    if len(name) == 0:
        if len(prefix) == 0:
            raise KeyNameException("Empty name is invalid")

        return prefix

    # Then find and validate the Namespace
    namespace = None
    namespace_offset = 0

    # This will contain the Key Name without the Namespace (and Namespace separator `:`)
    fullname = None

    # Namespace is everything before the first colon `:`, or empty if there is no colon `:`
    if len(prefix) == 0:
        # No prefix, so we look in name
        colon_index = name.find(":")
        slash_index = name.find("/")

        # There was a slash before the first colon, but not at the start of the string
        # -> this is not a valid Key Name
        if slash_index < colon_index and slash_index != 0:
            raise KeyNameException(
                "No namespace (and not cascading) or missing colon (:) after namespace"
            )

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
        # We have a prefix, so we extract the Namespace from there
        colon_index = prefix.find(":")
        slash_index = name.find("/")

        # There was a slash before the first colon, but not at the start of the string
        # -> this is not a valid Key Name
        if slash_index < colon_index and slash_index != 0:
            raise KeyNameException(
                "No namespace (and not cascading) or missing colon (:) after namespace"
            )

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

    # Check if Key Name starts correctly after Namespace
    if len(fullname) == 0 or fullname[0] != "/":
        raise KeyNameException(
            f"Key must start with '<NAMESPACE>:/' or just '/'. " +
            f"Allowed values for <NAMESPACE>: {NAMESPACES}"
        )

    # Check if Key Name starts correctly after Namespace
    if fullname == "/%":
        raise KeyNameException(
            f"Key must NOT be '<NAMESPACE>:/%' or just '/%' (collides with root key). " +
            f"Allowed values for <NAMESPACE>: {NAMESPACES}"
        )

    # Check for dangling escapes
    if sum(1 for _ in takewhile(lambda x: x == "\\", reversed(fullname))) % 2 != 0:
        raise KeyNameException(
            f"The key must not end with an unescaped backslash '\\'."
        )

    # Split Key Name into Key Name Parts and process separately
    parts = list(PART_REGEX.finditer(fullname))
    part_count = len(parts)

    # Will contain a Tuple for each Key Name Part in the canonical Key Name.
    # The first element of the Tuple is the RegEx Mach object. This is used for processing `..` parts.
    # The second element is either None, meaning this part will be skipped when creating the final Key Name or a str, i.e. a canonical Key Name Part.
    key_parts: List[Tuple[Match[str], Optional[str]]] = []

    for index, part in enumerate(parts):
        full_part = part.group(0)
        leading_slashes = len(part.group('leadingSlashes'))
        actual_part = part.group('content')

        part_start = part.start(0) + namespace_offset
        part_end = part.end(0) + namespace_offset

        # Report leading slashes in verbose mode.
        if verbose and leading_slashes > 0:
            print(
                f"[DEBUG] removing {leading_slashes} leading slashes " +
                f"from part '{full_part}' ({part_start}:{part_end})",
                file=sys.stderr
            )

        # If this is the last part and we have no content, we are done. A trailing slash is explicitly allowed.
        if index == part_count - 1:
            if len(actual_part) == 0:
                if verbose:
                    print(
                        f"[DEBUG] removing empty last part ({part_start}:{part_end})",
                        file=sys.stderr
                    )
                break

        # Validate escape sequences
        for esc in ESCAPE_REGEX.finditer(actual_part):
            escape = esc.group(1)

            escape_valid = escape in ESCAPES or (
                escape in ESCAPES_SPECIAL and
                ESCAPES_SPECIAL[escape](actual_part)
            )
            if not escape_valid:
                raise KeyNameException(
                    f"Illegal escape sequence '\\{escape}'. " +
                    f"Invalid part '{full_part}' ({part_start}:{part_end})."
                )

        if actual_part == ".":
            # A `.` is skipped for the canonical Key Name.
            if verbose:
                print(
                    f"[DEBUG] skipping part '.' ({part_start}:{part_end})",
                    file=sys.stderr
                )
        elif actual_part == "..":
            # A `..` removes a part from the canonical Key Name, so we need to check if there is a part left.
            # If there is no part left, `..` behaves like `.`
            if len(key_parts) == 0:
                print(
                    f"[DEBUG] ignoring additional part '..' ({part_start}:{part_end})",
                    file=sys.stderr
                )
                continue

            if verbose:
                previous_part, _ = key_parts[-1]
                full_previous_part = previous_part.group(0)
                previous_start = previous_part.start(0) + namespace_offset
                previous_end = previous_part.end(0) + namespace_offset
                print(
                    f"[DEBUG] found part '..' ({part_start}:{part_end}) " +
                    f"removing previous part '{full_previous_part}' ({previous_start}:{previous_end})",
                    file=sys.stderr
                )

            key_parts.pop()
        elif actual_part[0] == "#":
            # This may be an Array part, so we need to check the structure.
            valid, digits = check_array_part(actual_part)

            if not valid:
                key_parts.append((part, actual_part))
                continue

            # The canonical version of an Array Part always has the underscores.
            canonical_part = f"#{'_' * (len(digits) - 1)}{digits}"

            if verbose:
                print(
                    f"[DEBUG] found array part '{full_part}'" +
                    f" canoncalized to '{canonical_part}' ({part_start}:{part_end})",
                    file=sys.stderr
                )

            key_parts.append((part, canonical_part))
        elif actual_part[0] == "%":
            # Parts starting with `%` are reserved, ...
            if verbose:
                if len(actual_part) == 1:
                    print(
                        f"[DEBUG] part '%' represents empty part ({part_start}:{part_end})",
                        file=sys.stderr
                    )

            # ... but do not make Key Names invalid.
            key_parts.append((part, actual_part))
        else:
            # Basic Key Name Part, just append to list
            key_parts.append((part, actual_part))

    # Create the Key Name without the namespace
    key = "/".join(p for (_, p) in key_parts if p is not None)

    # Add the Namespace, if this is not a cascading Key Name
    if namespace is not None:
        fullkey = f"{namespace}:/{key}"
    else:
        fullkey = f"/{key}"

    return fullkey


def unescape(canonical: str, verbose: bool = False) -> Tuple[Namespace, Iterator[str]]:
    """
    Returns the Namespace enum value and list of Key Name Parts for a given canonical Key Name.

    `canonical` MUST be a canonical Key Name. If you are not sure about this, call `canonicalize` first.
    """

    # Extract the Namespace string value and convert it into an enum value.
    colon_index = canonical.find(":")
    if colon_index > 0:
        namespace = NAMESPACES_MAP[canonical[:colon_index]]
    else:
        namespace = Namespace.CASCADING

    def unescape_part(part: Match[str]) -> str:
        """
        Returns the unescaped version of a single Key Name Part.

        `part` MUST NOT contain unescaped slashes `/` for this function to be correct.
        """

        actual_part = part.group("content")

        if actual_part == "%":
            # Found an empty part, replace with empty string
            if verbose:
                part_start = part.start(0)
                part_end = part.end(0)
                print(
                    f"[DEBUG] '%' represents an empty part. ({part_start}:{part_end})",
                    file=sys.stderr
                )

            return ""
        else:
            # Simply remove the backslashes `\` from the escape sequences
            return ESCAPE_REGEX.sub("\\1", actual_part)

    # Use the RegEx to separate the parts and then unescape each one individually
    unescaped = (
        unescape_part(part) for part in PART_REGEX.finditer(canonical)
    )

    return (namespace, unescaped)


"""
START OF MAIN FUNCTION
"""

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Check and convert Elektra Key Names."
    )
    parser.add_argument(
        "--verbose", "-v",
        help="Show more info about what is happening.",
        action="store_true"
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

    args = parser.parse_args()

    if args.command == "canonicalize":
        try:
            if "prefix" in args:
                key_canonical = canonicalize(
                    args.name,
                    args.prefix,
                    verbose=args.verbose
                )
            else:
                key_canonical = canonicalize(
                    args.name,
                    verbose=args.verbose
                )

            print(key_canonical)
        except KeyNameException as e:
            print("ERROR:", e, file=sys.stderr)
            exit(1)
    elif args.command == "unescape":
        try:
            if "prefix" in args:
                key_canonical = canonicalize(args.name, args.prefix)
            else:
                key_canonical = canonicalize(args.name)

            if args.verbose:
                print(f"[DEBUG] canonical version of name: {key_canonical}")

            namespace, unescaped = unescape(
                key_canonical,
                verbose=args.verbose
            )

            if args.raw:
                sys.stdout.buffer.write(
                    bytes([namespace.value]) +
                    b"\0" +
                    b"\0".join(bytes(p, encoding="utf-8") for p in unescaped) +
                    b"\0"
                )
            elif args.python_bytes:
                print(
                    bytes([namespace.value]) +
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
