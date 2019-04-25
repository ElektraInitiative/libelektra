- infos = Information about the hexcolor plugin is in keys below
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/hexcolor
- infos/description = Validation of hexcolors

## Introduction

Copy this hexcolor if you want to start a new
plugin written in C.

## Usage

- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies

None.

## Examples

```sh
# Mount a config file with the hexcolor plugin
sudo kdb mount color.ecf user/tests/color/hex dump hexcolor

# Suceeds, since the value is a valid hexcolor. Quotes are important!
kdb set user/tests/color/hex "#fff"

# Check the user/tests/color/hex key for validity
kdb setmeta user/tests/color/hex check/hexcolor any

# Suceeds, since the value is a valid hexcolor. Quotes are important!
kdb set user/tests/color/hex "#a1C2b3"

# Suceeds, since the value is a valid RGBA hexcolor. Quotes are important!
kdb set user/tests/color/hex "#aabbccdd"

# Colors are normalized to bytes
kdb get user/tests/color/hex
#> \xaa\xbb\xcc\xdd

# Try to set incorrect value
kdb set user/tests/color/hex fff
# RET: 5

# Try to set incorrect value
kdb set user/tests/color/hex "fff"
# RET: 5

# Try to set incorrect value
kdb set user/tests/color/hex "#12345"
# RET: 5

# Undo modifications to the key database
kdb rm -r user/tests/color/hex
sudo kdb umount user/tests/color/hex
```

## Limitations

None.
