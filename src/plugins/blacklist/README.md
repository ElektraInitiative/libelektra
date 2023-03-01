- infos = Information about the blacklist plugin is in keys below
- infos/author = Robert Sowula <robert@sowula.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/blacklist/#
- infos/description = blacklisting key values

## Introduction

This plugin is a blacklist plugin that blocks the assignment of specific values to a key.

## Usage

The plugin looks for the metadata array `check/blacklist/#` and reject all matching key values.

For example:

```
check/blacklist = #2
check/blacklist/#0 = water
check/blacklist/#1 = fire
check/blacklist/#2 = air
```

All values in the array will be rejected. The array indices don't have to be continuous, using e.g. only `#1`, `#2` and
`#4` is also allowed. Just make sure `check/blacklist` is set to the largest index in the array.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Examples

```sh
sudo kdb mount blacklist.ecf /tests/blacklist blacklist

# valid initial value + setup valid blacklist list
kdb set user:/tests/blacklist ""
kdb set user:/tests/blacklist/value water
kdb meta set spec:/tests/blacklist/value check/blacklist '#2'
kdb meta set spec:/tests/blacklist/value check/blacklist/#0 fire
kdb meta set spec:/tests/blacklist/value check/blacklist/#1 air
kdb meta set spec:/tests/blacklist/value check/blacklist/#2 cold/water

# should succeed
kdb set user:/tests/blacklist/value earth

# should fail
kdb set user:/tests/blacklist/value fire
# RET:5
# ERROR:C03200

# should fail
kdb set user:/tests/blacklist/value air
# RET:5
# ERROR:C03200

# should fail
kdb set user:/tests/blacklist/value cold/water
# RET:5
# ERROR:C03200
```

It is also possible to blacklist empty values:

```sh
kdb set user:/tests/blacklist/empty water
kdb meta set spec:/tests/blacklist/empty check/blacklist '#0'
kdb meta set spec:/tests/blacklist/empty check/blacklist/#0 ''

# should succeed
kdb set user:/tests/blacklist/empty earth

# should fail
kdb set user:/tests/blacklist/empty ''
# RET:5
# ERROR:C03200

# Undo changes
kdb rm -rf spec:/tests/blacklist
kdb rm -r user:/tests/blacklist || kdb rm -r system:/tests/blacklist
# sudo kdb umount spec:/tests/blacklist
sudo kdb umount /tests/blacklist
```
