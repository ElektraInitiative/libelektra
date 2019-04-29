- infos = Information about the rgbcolor plugin is in keys below
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/rgbcolor
- infos/description = Validation of rgbcolors

## Introduction

tba

## Usage

- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies

None.

## Examples

```sh
# Mount a config file with the rgbcolor plugin
sudo kdb mount color.ecf user/tests/color/hex dump rgbcolor

# Suceeds, since the value is a valid rgbcolor. Quotes are important!
kdb set user/tests/color/hex "#fff"

# Check the user/tests/color/hex key for validity
kdb setmeta user/tests/color/hex check/rgbcolor any

# Suceeds, since the value is a valid rgbcolor. Quotes are important!
kdb set user/tests/color/hex "#a1C2b3"

# Suceeds, since the value is a valid RGBA rgbcolor. Quotes are important!
kdb set user/tests/color/hex "#aabbccdd"

# Colors are normalized to bytes
kdb get user/tests/color/hex
#> \xaa\xbb\xcc\xdd

kdb set user/tests/color/hex2 "#abc"
kdb setmeta user/tests/color/hex2 check/rgbcolor any

# Expanded to rgba: #aabbccff
kdb get user/tests/color/hex2
#> \xaa\xbb\xcc\xff

kdb set user/tests/color/hex2 "#abcd"

# Expanded to rgba: #aabbccdd
kdb get user/tests/color/hex2
#> \xaa\xbb\xcc\xdd

kdb set user/tests/color/hex2 "#aabbcc"

# Expanded to rgba: #aabbccff
kdb get user/tests/color/hex2
#> \xaa\xbb\xcc\xff

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
