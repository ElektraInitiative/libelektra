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
sudo kdb mount /tests/color.ini /tests/ipaddr ini hexcolor
#> Mount a config file with the hexcolor plugin

kdb setmeta /tests/color/hex check/hexcolor any
#> Check the /tests/color/hex key for validity

kdb set /tests/color/hex "#fff"
#> Suceeds, since the value is a valid hexcolor. Quotes are important!

kdb set /tests/color/hex "#a1C2b3"
#> Suceeds, since the value is a valid hexcolor. Quotes are important!

kdb set /tests/color/hex fff
#> Throws an error: value of key is not a valid hex-formatted color

kdb set /tests/color/hex "fff"
#> Throws an error: value of key is not a valid hex-formatted color

kdb set /tests/color/hex "#12345"
#> Throws an error: value of key is not a valid hex-formatted color
```

## Limitations

None.
