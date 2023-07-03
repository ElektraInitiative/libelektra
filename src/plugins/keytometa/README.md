- infos = Information about keytometa plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/provides = conv
- infos/needs =
- infos/placements = presetstorage postgetstorage
- infos/status = unittest tested nodep libc discouraged
- infos/metadata =
- infos/description = conversion of keys to metakeys and vice versa

## Introduction

> Note: This plugin uses a deprecated way to store comments.

This plugin converts keys into metakeys of other keys.
The keys to be converted are tagged with special metadata.
Converting keys into metakeys basically raises two questions:

- which keys should be converted
- which key to append the resulting metakeys to

The keys to be converted are identified by metakeys below `convert` (e.g. `convert/append`).
The keys receiving the resulting metadata are identified by append strategies.
The plugin currently supports the following metakeys for controlling the conversion:

- `convert/metaname` specifies the name of the resulting metakey. For example tagging the key `user:/config/key1` with `convert/metaname = comment` means that the key will be converted to a metakey with the name `comment`.
- `convert/append` specifies the append strategy (see below)
- `convert/append/samelevel` specifies that the key should only be written to the metadata of a key with the same hierarchy level (see below).

The keys converted to metadata are restored as soon as the keyset is written back.
However, the plugin is stateful. This means that a keyset must be read and keys must be
converted by the plugin in order to undo this conversion in the set direction.
Modifications to the metadata which resulted from converted keys are propagated back
to the corresponding key (see merging for more details).

The keys are ordered by the "order" metadata. If two keys are equal according to the order metadata,
they are ordered by name instead.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Append Strategies

The append strategy specifies which key will receive the resulting metadata.
Currently the plugin supports the following strategies:

### Parent Strategy

The metadata is added to the first existing parent of the converted key.
This does not necessarily have to be the parent of the keyset. If no such key is found,
the first key in a sorted keyset will receive the metadata (this is usually the parent key of the keyset).
For example consider the following keyset:

```
user:/config/key1
user:/config/key1/child1
user:/config/key2
user:/config/key2/deeper/child2
user:/config/child3
```

If child1, child2 and child3 were tagged with `convert/append = parent`, key1 would receive
the metadata from child1 and child3. Key2 would receive the metadata from child2.

### Next Strategy

The metadata is added to the key following the converted key in a sorted keyset.
If no such key is found (for example because the key to be converted is the last one),
the strategy is reverted to parent. For example consider the following keyset:

```
user:/config/deeper/key1
user:/config/key2
user:/config/key3
user:/config/key4
```

If key1 and key3 were tagged with `convert/append = next`, key2 would receive the metadata
resulting from key1 and key4 would receive the metadata resulting from key3.

### Previous Strategy

The metadata is added to the key preceding the converted key in a sorted keyset.
If no such key is found (for example because the key to be converted is the first one),
the strategy is reverted to parent. For example consider the following keyset:

```
user:/config/key1
user:/config/deeper/key2
user:/config/key3
user:/config/key4
```

If key2 and key4 were tagged with `convert/append = previous`, key1 would receive the metadata
resulting from key2 and key3 would receive the metadata resulting from key4.

## Merging

The metadata resulting from a converted key is never appended to another key which is going to
be converted. This prevents that the data of converted keys is invisible after the conversion.
Instead the metadata resulting from different converted keys with the same append strategy is
merged together (separated by a newline). Keys with different append strategies are skipped,
until either a key with the same strategy is found (which is simply merged as described above)
or the target key is found. The keys are always processed in the order of an ordered keyset.
For example consider the following keyset:

```
user:/config/key0
user:/config/key1 = value1
user:/config/key2 = value2
user:/config/key3 = value3
user:/config/key4 = value4
user:/config/key5
```

If key1 and key2 were tagged with `convert/append = next` and key3 and key4 were tagged with `convert/append = previous` the following would happen:

- the resulting metadata of key0 would contain `value3\nvalue4` (the values of key3 and key4 are merged together and key1 and key2 are skipped, as they have different append strategy)
- the resulting metadata of key5 would contain `value1\nvalue2` (the values of key1 and key2 are merged together and key3 and key4 are skipped, as they have different append strategy)

### Same-Level Appending

The option `convert/append/samelevel` can be used to force that the metadata is only appended to a key on the same hierarchy level. If no such key is found, the strategy is reverted to parent. Note, that the value of the samelevel key does not matter. Only its existence is relevant. For example consider the following keyset:

```
user:/config/key0
user:/config/key1/child1
user:/config/key2
user:/config/key3/child2
user:/config/key4
user:/config/key5
user:/config/key6
```

If child1, child2 and key4 were each tagged with `convert/append = next` and child2 and key4 were tagged with `convert/append/samelevel`, key2 would receive the metadata resulting from child1.
key0 would receive the metadata resulting from child2 (strategy reverted to parent, as the samelevel request cannot be fulfilled).
key5 would receive the metadata resulting from key4.

## Real World Example

The keytometa plugin was initially developed to aid the integration of the Augeas plugin. The Augeas plugin represents comments in configuration files as keys. However,
in Elektra comments are usually represented within comment metakeys. Therefore it would be desirable to convert all comment keys to comment metakeys. This is achieved
by adding the following to the Augeas plugin contract.

```c
// ...
keyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1",
    KEY_VALUE, "*#comment*",
    KEY_META, "convert/metaname", "comment/#0",
    KEY_META, "convert/append", "next",
    KEY_END),
keyNew ("system:/elektra/modules/augeas/config/needs/glob/get/#1/flags",
    KEY_VALUE, "", /* disable the path matching mode */
    KEY_END)
// ...
;
```

Tagging the keys to be converted to comment metakeys happens via the glob plugin. The metadata set on the key `glob/get/#1` is copied to each key that matches the
pattern `*#comment*`, i.e. each comment key generated by the Augeas plugin. `convert/metaname = comment` because we want the comment keys to be converted to the
comment metadata. `convert/append = next` is chosen because usually comments occur before the key they describe.
