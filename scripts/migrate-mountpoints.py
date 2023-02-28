#!/usr/bin/env python3

import argparse
import sys
from io import FileIO
from typing import Dict, Optional, Set, Tuple, Union

from dataclasses import dataclass

from kdb.kdb import Key, KeySet

PARENT = "system:/elektra"


def parse_dump(file: FileIO) -> KeySet:
    if file.readline() != b"kdbOpen 2\n":
        raise Exception("not a dump file")

    keys = KeySet()
    lastKey: Key
    for line in file:
        if line.startswith(b"$key"):
            [_, type, nsizestr, vsizestr] = line.split(b" ")
            nsize = int(nsizestr)
            vsize = int(vsizestr)

            name = file.read(nsize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            value = file.read(vsize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            lastKey = Key(f"{PARENT}/{str(name, encoding='utf-8')}")

            if type == b"binary":
                lastKey.value = value
            else:
                lastKey.value = str(value, encoding="utf-8")

            keys.append(lastKey)

        elif line.startswith(b"$meta"):
            [_, nsizestr, vsizestr] = line.split(b" ")
            nsize = int(nsizestr)
            vsize = int(vsizestr)

            name = file.read(nsize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            value = file.read(vsize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            lastKey.setMeta(str(name,  encoding="utf-8"),
                            str(value, encoding="utf-8"))

        elif line.startswith(b"$copymeta"):
            [_, nsizestr, msizestr] = line.split(b" ")
            nsize = int(nsizestr)
            msize = int(msizestr)

            name = file.read(nsize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            meta = file.read(msize)
            if file.read(1) != b"\n":
                raise Exception("invalid dump file")

            lastKey.copyMeta(keys.lookup(name), meta)
        elif line.startswith(b"$end"):
            break
        else:
            raise Exception("invalid dump file")

    return keys


@dataclass
class OldPlugin:
    label: str
    placement: int
    name: str


@dataclass
class Plugin:
    label: str
    positions: Set[str]
    name: str
    config: KeySet


PLACEMENTS = {
    "getplugins": [
        "resolver",
        "prestorage/#",
        "prestorage/#",
        "prestorage/#",
        "prestorage/#",
        "storage",
        "poststorage/#",
        "poststorage/#",
        "poststorage/#",
        "poststorage/#",
    ],
    "setplugins": [
        "resolver",
        "prestorage/#",
        "prestorage/#",
        "prestorage/#",
        "prestorage/#",
        "storage",
        "precommit/#",
        "commit",
        "postcommit/#",
        "postcommit/#",
    ],
    "errorplugins": [
        "prerollback/#",
        "prerollback/#",
        "prerollback/#",
        "prerollback/#",
        "prerollback/#",
        "rollback",
        "postrollback/#",
        "postrollback/#",
        "postrollback/#",
        "postrollback/#",
    ]
}

PLUGIN_INDEX = 0


def parse_plugin(pstr: str, others: Dict[str, OldPlugin]) -> Tuple[OldPlugin, Optional[str]]:
    if len(pstr) < 2 or pstr[0] != "#":
        raise Exception("illegal pstr")

    placement = int(pstr[1])

    if pstr[2] != "#":
        PLUGIN_INDEX += 1
        return (OldPlugin(f"unnamed{PLUGIN_INDEX}", placement, pstr[2:]), f"unnamed{PLUGIN_INDEX}")

    if pstr[-1] == "#":
        [name, label] = pstr[3:-1].split("#", 1)
        return (OldPlugin(label, placement, name), label)
    else:
        other = others[pstr[3:]]
        return (OldPlugin(other.label, placement, other.name), None)


def convert_config(old: KeySet, old_parent: Key, new_parent: Key) -> KeySet:
    new = KeySet()
    for o in old:
        n = Key(o.dup())
        n.name = f"{new_parent.name}/{o.name[len(old_parent.name)+1:]}"
        new.append(n)
    return new


def convert_mp(data: KeySet, base: str) -> KeySet:
    new_data = KeySet()

    mountpoint = data[f"{base}/mountpoint"].value

    def build_key(relative: str, value: Optional[Union[str, bytes]] = None) -> Key:
        k = Key(PARENT)
        k.addBaseName("mountpoints")
        k.addBaseName(mountpoint)
        if len(relative) > 0:
            k.addName(relative)
        if value is not None:
            k.value = value
        return k

    new_data.append(build_key(""))
    new_data.append(build_key("config"))

    new_data.append(build_key("plugins/backend"))
    new_data.append(build_key("plugins/backend/name", value="backend"))

    path = data[f"{base}/config/path"].value
    new_data.append(build_key("definition/path", value=path))

    for c in data.filter(lambda k: k.isBelow(Key(f"{base}/config"))):
        n = c.name[len(f"{base}/"):]
        if n == "config/path":
            continue

        k = build_key(n)
        k.value = c.value
        k.copyAllMeta(c)
        new_data.append(k)

    old_plugins: Dict[str, OldPlugin] = {}

    plugins: Dict[str, Plugin] = {}

    def process_plugins(oldtype: str, newtype: str):
        for p in data.filter(lambda k: k.isDirectBelow(Key(f"{base}/{oldtype}"))):
            (plugin, label) = parse_plugin(p.basename, old_plugins)

            position = PLACEMENTS[oldtype][plugin.placement]

            if label is not None:
                old_plugins[label] = plugin

                plugins[plugin.label] = Plugin(
                    plugin.label,
                    {f"{newtype}/{position}"},
                    plugin.name,
                    convert_config(
                        data.filter(lambda k: k.isBelow(
                            Key(f"{p.name}/config"))),
                        p,
                        Key("/"),
                    )
                )
            else:
                plugins[plugin.label].positions.add(f"{newtype}/{position}")

    process_plugins("errorplugins", "set")
    process_plugins("getplugins", "get")
    process_plugins("setplugins", "set")

    for (label, plugin) in plugins.items():
        new_data.append(build_key(f"plugins/{label}"))
        new_data.append(build_key(f"plugins/{label}/name", value=plugin.name))

        if len(plugin.config) > 0:
            new_data.append(build_key(f"plugins/{label}/config"))
        for c in plugin.config:
            m = Key(c.dup())
            m.name = build_key(f"plugins/{label}").name + m.name
            new_data.append(m)

        for pos in plugin.positions:
            if pos.endswith("#"):
                index = len(new_data.filter(lambda k: k.name.startswith(
                    build_key(f"definition/positions/{pos}").name
                )))
                indexstr = f"{'_'*(len(str(index))-1)}{index}"
                position = pos + indexstr
            else:
                position = pos

            new_data.append(
                build_key(f"definition/positions/{position}", value=label)
            )

    return new_data


def convert(data: KeySet) -> KeySet:
    new_data = KeySet()
    parent = Key(f"{PARENT}/mountpoints")
    for key in data:
        if key.isBelow(parent):
            if key.isDirectBelow(parent):
                new_mp = convert_mp(data.filter(
                    lambda k: k.isBelow(key)), key.name)
                new_data.append(new_mp)
        else:
            new_data.append(key)
    return new_data


def format_dump(out: FileIO, keys: KeySet):
    out.write(b"kdbOpen 2\n")
    for key in keys:
        key: Key
        type = "binary" if key.isBinary() else "string"
        nsize = len(key.name) - len(PARENT) - 1

        out.write(
            bytes(f"$key {type} {nsize} {len(key.value)}\n", encoding="utf-8"))
        out.write(bytes(key.name[len(PARENT)+1:], encoding="utf-8"))
        out.write(b"\n")
        if key.isBinary():
            out.write(key.value)
        else:
            out.write(bytes(key.value, encoding="utf-8"))
        out.write(b"\n")

        for meta in key.getMeta():
            meta: Key
            msize = len(meta.name) - len('meta:/')

            out.write(
                bytes(f"$meta {msize} {len(meta.value)}\n", encoding="utf-8"))
            out.write(bytes(meta.name[len(b"meta:/"):], encoding="utf-8"))
            out.write(b"\n")
            out.write(meta.value)
            out.write(b"\n")

    out.write(b"$end\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="migrate-mountpoints")
    parser.add_argument("file",
                        nargs="?",
                        default="/etc/kdb/elektra.ecf",
                        type=argparse.FileType("rb"),
                        )
    parser.add_argument("--output",
                        default=sys.stdout.buffer,
                        type=argparse.FileType("wb"),
                        )

    args = parser.parse_args()
    data = parse_dump(args.file)

    new_data = convert(data)
    format_dump(args.output, new_data)
