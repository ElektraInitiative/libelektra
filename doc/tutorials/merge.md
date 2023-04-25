# How-To: kdb merge

## Introduction

The kdb tool allows users to access and perform functions on the Elektra Key Database from the command line. We added
a new command to this very useful tool, the `merge` command. This command allows a user to perform a three-way merge
of KeySets from the `kdb` tool.

The command to use this tool is:

```sh
kdb merge [options] ourpath theirpath basepath resultpath
# RET: 7
```

The standard naming scheme for a three-way merge consists of `ours`, `theirs`, and `base`:

- `ours` refers to the local copy of a file
- `theirs` refers to a remote copy
- `base` refers to their common ancestor.

This works very similarly for KeySets, especially ones that consist of mounted configuration files.

For mounted configuration files:

- `ours` should be the user's copy
- `theirs` would be the maintainers copy,
- `base` would be the previous version of the maintainer's copy.

If the user is just trying to accomplish a three-way merge using any two arbitrary keysets that share a base,
it doesn't matter which ones are defined as `ours` or `theirs` as long as they use the correct base KeySet.
In `kdb merge`, `ourpath`, `theirpath`, and `basepath` work just like `ours`, `theirs`, and `base` except each one represents the
root of a KeySet. The argument `resultpath` is pretty self-explanatory, it is just where you want the result of the merge to be saved under.
It's worth noting, `resultpath` should be empty before attempting a merge, otherwise there can be unintended consequences.

## Options

As for the options, there are two basic options:

- `-i`, `--interactive`: which attempts the merge in an interactive way
- `-f`, `--force`: which overwrites any Keys in `resultpath`

### Strategies

Additionally, there is an option to specify a merge strategy, which is very important.

The option for strategy is:

- `-s <name>`, `--strategy <name>`: which is used to specify a strategy to use in case of a conflict

The current list of strategies are:

- `preserve`: the merge will fail if a conflict is detected
- `ours`: the merge will use our version during a conflict
- `theirs`: the merge will use their version during a conflict
- `cut`: Removes existing keys below the resultpath and replaces them with the merged keyset.
- `import`: (DEPRECATED, avoid using it!)
  Preserves existing keys in the resultpath if they do not exist in the merged keyset.
  If the key does exist in the merged keyset, it will be overwritten.

If no strategy is specified, the merge will default to the preserve strategy as to not risk making the wrong decision.
If any of the other strategies are specified, when a conflict is detected, merge will use the Key specified by the
strategy (`ours`, `theirs`, `cut` or `import`) for the resulting Key.

## Basic Example

Basic Usage:

```sh
kdb merge system:/hosts/ours system:/hosts/theirs system:/hosts/base system:/hosts/result
```

## Examples Using Strategies

Here are examples of the same KeySets being merged using different strategies.
The KeySets are mounted using a property format, the left side of '=' is the name of
the Key, the right side is its string value.

We start with the base KeySet, `system:/base`:

```ini
key1=1
key2=2
key3=3
key4=4
key5=5
```

Here is our KeySet, `system:/ours`:

```ini
key1=apple
key2=2
key3=3
key5=fish
```

Here is their KeySet, `system:/theirs`:

```ini
key1=1
key2=pie
key4=banana
key5=5
```

To add the keys to the key database, execute the following commands:

```sh
kdb set user:/tests/base/key1 1
#> Create a new key user:/tests/base/key1 with string "1"

kdb set user:/tests/base/key2 2
#> Create a new key user:/tests/base/key2 with string "2"

kdb set user:/tests/base/key3 3
#> Create a new key user:/tests/base/key3 with string "3"

kdb set user:/tests/base/key4 4
#> Create a new key user:/tests/base/key4 with string "4"

kdb set user:/tests/base/key5 5
#> Create a new key user:/tests/base/key5 with string "5"


kdb set user:/tests/ours/key1 apple
#> Create a new key user:/tests/ours/key1 with string "apple"

kdb set user:/tests/ours/key2 2
#> Create a new key user:/tests/ours/key2 with string "2"

kdb set user:/tests/ours/key3 3
#> Create a new key user:/tests/ours/key3 with string "3"

kdb set user:/tests/ours/key5 fish
#> Create a new key user:/tests/ours/key5 with string "fish"


kdb set user:/tests/theirs/key1 1
#> Create a new key user:/tests/theirs/key1 with string "1"

kdb set user:/tests/theirs/key2 pie
#> Create a new key user:/tests/theirs/key2 with string "pie"

kdb set user:/tests/theirs/key4 banana
#> Create a new key user:/tests/theirs/key4 with string "banana"

kdb set user:/tests/theirs/key5 5
#> Create a new key user:/tests/theirs/key5 with string "5"
```

Now we will examine the result KeySet with the different strategies.

### Preserve

```sh
kdb merge -s preserve user:/tests/ours user:/tests/theirs user:/tests/base user:/tests/result
# RET: 11
# STDERR: 1 conflicts were detected that could not be resolved automatically:⏎user:/tests/result/key4⏎ours: CONFLICT_DELETE, theirs: CONFLICT_MODIFY⏎⏎Merge unsuccessful.
```

The merge will fail because of a conflict for `key4` since `key4` was deleted in our KeySet and
edited in their KeySet. Since we used preserve, the merge fails and the result KeySet is not saved.

### Ours

```sh
kdb merge -s ours user:/tests/ours user:/tests/theirs user:/tests/base user:/tests/result
```

The result KeySet, `user:/tests/result` will be:

```sh
kdb ls user:/tests/result
#> user:/tests/result/key1
#> user:/tests/result/key2
#> user:/tests/result/key5
```

The values of the keys are:

```sh
kdb get user:/tests/result/key1
#> apple

kdb get user:/tests/result/key2
#> pie

kdb get user:/tests/result/key5
#> fish
```

The conflict of `key4` (it was deleted in `ours` but changed in `theirs`) is solved by using our copy, thus deleting the key.

Now we delete the result keys and try the next merging strategy.

```sh
kdb rm user:/tests/result/key1
kdb rm user:/tests/result/key2
kdb rm user:/tests/result/key5
```

### Theirs

```sh
kdb merge -s theirs user:/tests/ours user:/tests/theirs user:/tests/base user:/tests/result
```

The result KeySet, `user:/tests/result` will be:

```sh
kdb ls user:/tests/result
#> user:/tests/result/key1
#> user:/tests/result/key2
#> user:/tests/result/key4
#> user:/tests/result/key5
```

The values of the keys are:

```sh
kdb get user:/tests/result/key1
#> apple

kdb get user:/tests/result/key2
#> pie

kdb get user:/tests/result/key4
#> banana

kdb get user:/tests/result/key5
#> fish
```

Here, the conflict of `key4` is solved by using their copy, thus `key4=banana`.

We delete the result keys again and finally try the `cut` merging strategy.

```sh
kdb rm user:/tests/result/key1
kdb rm user:/tests/result/key2
kdb rm user:/tests/result/key4
kdb rm user:/tests/result/key5
```

### Cut

```sh
kdb merge -s cut user:/tests/ours user:/tests/theirs user:/tests/base user:/tests/result
```

The result KeySet, `user:/tests/result` will be:

```sh
kdb ls user:/tests/result
#> user:/tests/result/key1
#> user:/tests/result/key2
#> user:/tests/result/key4
#> user:/tests/result/key5
```

The values of the keys are:

```sh
kdb get user:/tests/result/key1
#> 1

kdb get user:/tests/result/key2
#> pie

kdb get user:/tests/result/key4
#> banana

kdb get user:/tests/result/key5
#> 5
```

Here the state of theirs is simply copied to the `resultpath`.

## SEE ALSO

- [kdb-merge(1)](../help/kdb-merge.md)
- [elektra-merge-strategy(7)](../help/elektra-merge-strategy.md)
