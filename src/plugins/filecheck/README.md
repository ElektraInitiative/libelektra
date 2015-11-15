- infos = Information about the filecheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage precommit
- infos/description =

## Introduction ##

The filecheck plugin validates files testing: encoding, lineendings, BOM, printable characters and null bytes.

## Configuration ##

`checkLE`
`validLE`
When the `checkLE` key is present, the plugin checks the file for consistent line endings. If you want to validate for a specific line ending you can supply it with the `validLE` key. Valid values are: `CR`, `LF`, `CRLF`, `LFCR`.

`rejectNull`
When the `rejectNull` key is present, the plugin rejects the file if a NULL-Byte is found. 

`checkEncoding`
`encoding`
When the `checkEncoding` key is present, the plugin validates the file encoding supplied by the key `encoding`, or, if not present, defaults to `UTF-8`

`rejectBom`
When the `rejectBom` key is present, the plugin rejects the file if any BOM markers are found.

`rejectUnprintable`
When the `rejectUnprintable` key is preset, the plugin rejects the file if an unprintable character is present (except `\r` and `\n`).

