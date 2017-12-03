- infos = Information about the directoryvalue plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained nodep preview experimental unfinished concept  discouraged
- infos/metadata =
- infos/description = This plugin converts directory values to leaf values

# Directory Value

## Introduction

The Directory Value plugin converts

1. directory (non-leaf) values to leaf values in the “set” direction, and
2. converts them back to directory values in the “get” direction.

A directory value is a key that contains children. For example in the key set:

```
user/grandparent                = Grandparent
user/grandparent/leaf           = Leaf
user/grandparent/parent         = Parent
user/grandparent/parent/child   = Child
user/mother                     = Mother
user/mother/daughter            = Daughter
user/mother/son                 = Son
```

the keys

```
user/grandparent
user/grandparent/parent
user/mother
```

represent directory values, while the keys

```
user/grandparent/leaf
user/grandparent/parent/child
user/mother/daughter
user/mother/son
```

specify leaf values. You can easily check this by drawing the key set in the form of a rooted tree:

```
             user
      /               \
  granparent        mother
  /      |          /    \
leaf  parent    daughter son
         |
       child
```

. The Directory Value plugin converts all directory values to leaf values in the “set” direction by adding new keys with the postfix
`___dirdata`. Theses keys then store the old value of their parent keys

```
user/grandparent                    =
user/grandparent/___dirdata         = Grandparent
user/grandparent/leaf               = Leaf
user/grandparent/parent             =
user/grandparent/parent/___dirdata  = Parent
user/grandparent/parent/child       = Child
user/mother                         =
user/mother/___dirdata              = Mother
user/mother/daughter                = Daugther
user/mother/son                     = Son
```

. You might ask why we need the Directory Value plugin at all. The reason why we created this plugin is that some storage plugins like
[`yajl`](../yajl/README.md) or [`yamlcpp`](../yajl/README.md) are only able to save values inside leaf keys. By loading the Directory Value
plugin these storage plugins are also able to represent directory values properly.

## Usage

To mount the plugin use the command:

```sh
# Mount plugin to cascading namespace `/examples/directoryvalue`
sudo kdb mount config.file /examples/directoryvalue directoryvalue
```

. To unmount the plugin use the command

```sh
sudo kdb umount /examples/directoryvalue
```

.

## Examples

```sh
# Mount plugin to cascading namespace `/examples/directoryvalue`
sudo kdb mount config.file /examples/directoryvalue directoryvalue

# Undo changes to the key database
sudo kdb umount /examples/directoryvalue
```

## Limitations

Currently this plugin does nothing useful.
