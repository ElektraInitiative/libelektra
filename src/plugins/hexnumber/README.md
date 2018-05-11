- infos = Information about the hexnumber plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = nodep unfinished
- infos/metadata = unit/base type
- infos/ordering = type
- infos/description = interprets strings with prefix 0x as hexadecimal numbers

## Introduction

This plugin can be used to read configuration files that use hexadecimal values. All "hex-values" (see below) will be 
converted into decimal when the Elektra reads values from the mounted file. When Elektra writes back to the file the converted values
will be converted back and stored as before (`0X` will be replaced with `0x`).

### What are "hex-values"?
There are multiple ways you can signal to the hexnumber plugin, that a value should be converted:

1. If a Key has the metadata `/unit/base` set to `hex` it will always be interpreted as a hex-value. The plugin will also produce an error,
   if the value contained in such a Key does not start with `0x` (or `0X`).
2. If a Key has the metadata `type` set to an integer-type and its value starts with `0x` (or `0X`) it will be interpreted as a hex-value.
   The following types are recognized as integer-types per default: `int`, `byte`, `short`, `unsigned_short`, `long`, `unsigned_long`, 
   `long_long`, `unsigned_long_long`
3. If forced conversion mode is enabled all values starting with `0x` (or `0X`) are considered hex-values.

## Configuration

When mounting a backend with the hexnumber plugin, a few parameters can be configured.

1. To enable forced conversion mode set `/force` to any value. In forced conversion mode the plugin converts **ALL** strings starting with 
   `0x` (or `0X`) into decimal before passing the value on to the rest of Elektra.

    ```
    sudo kdb mount test.ecf /examples/hexnumber/forced hexnumber /force=1
    ```

2. The types recognized as integers can be configured. For this purpose specify all types you want to be considered for possible hexadecimal
   conversion as a semicolon(;)-delimited list. This overwrites the list of default types completely.
   
   ```
   sudo kdb mount test.ecf /examples/hexnumber/customtypes hexnumber /integertypes=int;long;customint
   ```

## Usage

- To mount a simple backend that uses hexadecimal numbers, you can use:

    ```
    sudo kdb mount test.ecf /examples/hexnumber/test hexnumber
    ```

- To unmount the plugin use the following command:

    ```
    sudo kdb umount /examples/hexnumber/test
    ```

## Example
```sh
# Mount plugin
sudo kdb mount test.ecf user/examples/hexnumber/test dump hexnumber

# Set up examples
kdb set user/examples/hexnumber/test/hex 0x1F
kdb setmeta user/examples/hexnumber/test/hex type int

kdb set user/examples/hexnumber/test/dec 26
kdb setmeta user/examples/hexnumber/test/dec type int

kdb set user/examples/hexnumber/test/string value
kdb setmeta user/examples/hexnumber/test/string type string

kdb set user/examples/hexnumber/test/hex2 0x1F
kdb setmeta user/examples/hexnumber/test/hex2 unit/base hex

kdb set user/examples/hexnumber/test/hexerror 0xXFX
kdb setmeta user/examples/hexnumber/test/hexerror unit/base hex

# Example 1: read hex value
kdb get user/examples/hexnumber/test/hex
#> 31

# Example 2: decimal value not converted
kdb get user/examples/hexnumber/test/dec
#> 26

# Example 3: string untouched
kdb get user/examples/hexnumber/test/string
#> value

# Example 4: read hex value with unit/base
kdb get user/examples/hexnumber/test/hex2
#> 31

# Example 5: string untouched
kdb get user/examples/hexnumber/test/hexerror
#> value

# Undo changes
kdb rm -r user/examples/hexnumber/test
sudo kdb umount user/examples/hexnumber/test
```