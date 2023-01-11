- infos = Information about the filecheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage precommit
- infos/status = maintained tested/unit configurable
- infos/metadata =
- infos/description = validates files (e.g. encoding)

## Introduction

The filecheck plugin validates files. It tests: encoding, lineendings, BOM, printable characters and null bytes.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Configuration

`check/lineending`
`valid/lineending`
When the `check/lineending` key is present, the plugin checks the file for consistent line endings. If you want to validate for a specific line ending you can supply it with the `valid/lineending` key. Valid values are: `CR`, `LF`, `CRLF`, `LFCR`.

`check/encoding`
`valid/encoding`
When the `checkEncoding` key is present, the plugin validates the file encoding supplied by the key `encoding`, or, if not present, defaults to `UTF-8`

`reject/null`
When the `reject/null` key is present, the plugin rejects the file if a NULL-Byte is found.

`reject/bom`
When the `reject/bom` key is present, the plugin rejects the file if any BOM markers are found.

`reject/unprintable`
When the `reject/unprintable` key is preset, the plugin rejects the file if an unprintable character is present (except `\r` and `\n`).
