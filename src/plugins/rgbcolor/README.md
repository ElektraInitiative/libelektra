- infos = Information about the rgbcolor plugin is in keys below
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/ordering = type
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/rgbcolor
- infos/description = Validation and normalization of rgbcolors

## Introduction

This plugin validates hex-formatted rgb color strings and normalizes them to decimal rgba format. It also accepts [named colors](https://www.w3.org/TR/css-color-3/#svg-color) and normalizes them.

## Usage

Add the metakey `check/rgbcolor` with an arbitrary value (e.g. `""`) to the key that you want to check and normalize.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Examples

```sh
# Mount a config file with the rgbcolor plugin
sudo kdb mount color.ecf user:/tests/color dump rgbcolor

# Suceeds, since the value is a valid rgbcolor. Quotes are important!
kdb set user:/tests/color/hex "#a1C2b3"

# Tell the plugin to validate the key and normalize if necessary
kdb meta set user:/tests/color/hex check/rgbcolor ""

# Colors are normalized to 32-bit unsigned integers
# This one is normalized to 0xa1C2b3ff
kdb get user:/tests/color/hex
#> 2713891839

# Color names are supported (https://www.w3.org/TR/css-color-3/#svg-color)
kdb set user:/tests/color/hex "yellowgreen"

# yellowgreen is 0x9acd32ff
kdb get user:/tests/color/hex
#> 2597139199

kdb set user:/tests/color/hex/subcolor "#abc"
kdb meta set user:/tests/color/hex/subcolor check/rgbcolor ""

# Expanded to rgba: #aabbccff
kdb get user:/tests/color/hex/subcolor
#> 2864434431

kdb set user:/tests/color/hex/subcolor "#abcd"

# Expanded to rgba: #aabbccdd
kdb get user:/tests/color/hex/subcolor
#> 2864434397

kdb set user:/tests/color/hex/subcolor "#aabbcc"

# Expanded to rgba: #aabbccff
kdb get user:/tests/color/hex/subcolor
#> 2864434431

# Try to set incorrect value
kdb set user:/tests/color/hex fff
# RET: 5

# Try to set incorrect value
kdb set user:/tests/color/hex/subcolor "not a named color"
# RET: 5

# Try to set incorrect value
kdb set user:/tests/color/hex "fff"
# RET: 5

# Try to set incorrect value
kdb set user:/tests/color/hex "#12345"
# RET: 5

# Old values are still there
kdb get user:/tests/color/hex
#> 2597139199

# Undo modifications to the key database
kdb rm -r user:/tests/color
sudo kdb umount user:/tests/color
```
