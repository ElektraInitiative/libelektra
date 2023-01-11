- infos = Information about the hexnumber plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained tested/unit nodep configurable
- infos/metadata = unit/base
- infos/ordering = type
- infos/description = converts hexadecimal values into decimal and back

## Introduction

This plugin is used to read configuration files that use hexadecimal values. All "hex-values" (see below) will be
converted into decimal when Elektra reads values from the mounted file. When Elektra writes back to the file the converted values
will be converted back and stored as before (`0X` will be replaced with `0x`).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

### What are "hex-values"?

There are multiple ways you can signal to the hexnumber plugin, that a value should be converted:

1. If a Key has the metadata `unit/base` set to `hex` it will always be interpreted as a hex-value. The plugin will also produce an error,
   if the value contained in such a Key does not start with `0x` (or `0X`).
2. If `unit/base` is not present and
   - the `type` metadata is set to one of the recognized integer-types (default: `byte`, `short`, `unsigned_short`, `long`, `unsigned_long`,
     `long_long`, `unsigned_long_long`)
   - AND the configuration value itself starts with `0x` (or `0X`) it will be interpreted as a hex-value.
3. If forced conversion mode (`/force` plugin configuration, see below) is enabled all values starting with `0x` (or `0X`) are considered hex-values.

## Configuration

When mounting a backend with the hexnumber plugin, a few settings can be configured.

1. To enable forced conversion mode set `/force` to any value. In forced conversion mode the plugin tries to convert **ALL** strings
   starting with `0x` (or `0X`) into decimal before passing the value on to the rest of Elektra. This can be useful for importing a
   configuration file that uses hexadecimal values into Elektra without writing a specification for the file.

   NOTE: be careful when using this option, as any configuration value that contains invalid non-hexadecimal characters
   (i.e. does not match `0[xX][0-9A-Fa-f]+`) will result in an error.

   ```bash
   sudo kdb mount test.ecf /examples/hexnumber/forced hexnumber /force=1
   ```

2. The types recognized as integers can be configured. For this purpose specify all _additional_ types you want to be considered for
   possible hexadecimal conversion as an Elektra array. All keys with a type from `/accept/types/#`, or one of the default types, will
   be converted to hexadecimal if the value starts with `0x` (or `0X`).

   ```bash
   sudo kdb mount test.ecf /examples/hexnumber/customtypes hexnumber /accept/types/#0=customint /accept/types/#1=othercustomint
   ```

## Usage & Example

- To mount a simple backend that uses hexadecimal numbers, you can use:
  ```sh
  sudo kdb mount test.ecf user:/tests/hexnumber hexnumber
  ```
- A few examples on how to use the plugin:

  ```sh
  # Example 1: read hex value
  kdb set user:/tests/hexnumber/hex 0x1F
  kdb meta-set user:/tests/hexnumber/hex type long

  kdb get user:/tests/hexnumber/hex
  #> 31

  # Example 2: decimal value not converted
  kdb set user:/tests/hexnumber/dec 26
  kdb meta-set user:/tests/hexnumber/dec type long

  kdb get user:/tests/hexnumber/dec
  #> 26

  # Example 3: string untouched
  kdb set user:/tests/hexnumber/string value
  kdb meta-set user:/tests/hexnumber/string type string

  kdb get user:/tests/hexnumber/string
  #> value

  # Example 4: read hex value with unit/base
  kdb set user:/tests/hexnumber/hex2 0xF
  kdb meta-set user:/tests/hexnumber/hex2 unit/base hex

  kdb get user:/tests/hexnumber/hex2
  #> 15

  # Undo changes
  kdb rm -r user:/tests/hexnumber
  ```

- To unmount the plugin use the following command:
  ```sh
  kdb umount user:/tests/hexnumber
  #>
  ```
