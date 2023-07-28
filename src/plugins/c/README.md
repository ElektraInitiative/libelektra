- infos = Information about the c plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/maintainer = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs = ccode
- infos/provides = storage/c
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed unittest nodep libc writeonly preview
- infos/metadata =
- infos/description = C code KeySet exports for Elektra

## Usage

Export a subset of the KDB as C code which creates a KeySet (`ksNew(<number of keys>, keyNew(...), ...)`).
This can be useful for generating test data or help to generate code for already defined configuration data.

```sh
kdb export user:/testdata c
```

The output can be used in C code for generating KeySets and the keys inside them.
So one use case is to extract a part of the KDB and copy the generated code
to create the exported part of the KDB programmatically, e.g. for another Elektra installation.

It can also be useful if you develop your own application and first create the necessary
keys manually. Then you can export them from the KDB and just paste the generated code
in the right place of the codebase of your application.

> Please note that if you use this plugin for creating a mountpoint (e.g. by calling `kdb mount <filename> <key> c`),
> only the content written by the last `kdbSet (...)` which includes the mountpoint will be inside the file.
> If you call `kdbSet (...)` for such a mountpoint, the previous content of the file is erased.
>
> This is because the c plugin is implemented as a write-only plugin.
> It's recommended to only use it for exporting parts of the KDB by calling `kdb export <parent key>`.

## Example

In this example, we add some keys and metakeys to the KDB and export them with the c plugin.
The output can directly be used inside C source code which uses Elektra.

```sh
kdb set user:/tests/cplugin/key1 value1
#> Create a new key user:/tests/cplugin/key1 with string "value1"

kdb set user:/tests/cplugin/key2 value2
#> Create a new key user:/tests/cplugin/key2 with string "value2"

kdb meta-set user:/tests/cplugin/key2 metakey2.1 metaval2.1
kdb set user:/tests/cplugin/key2 metakey2.2 metaval2.2

kdb set user:/tests/cplugin/key3 value3
#> Create a new key user:/tests/cplugin/key3 with string "value3"

kdb export user:/tests/cplugin c
#> ksNew (3,
#>	keyNew ("user:/tests/cplugin/key1", KEY_VALUE, "value1", KEY_END),
#>	keyNew ("user:/tests/cplugin/key2", KEY_VALUE, "value2", KEY_META, "metakey2.1", "metaval2.1", KEY_META, "metakey2.2", "metaval2.2", KEY_END),
#>	keyNew ("user:/tests/cplugin/key3", KEY_VALUE, "value3", KEY_END),
#>	KS_END);
```
