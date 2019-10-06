# Arrays

## Key-Value Pairs

The main building block of Elektra’s database are hierarchical [key-value pairs](https://en.wikipedia.org/wiki/Key-value_database). You can create such a pair using [`kdb set`](../help/kdb-set.md):

```sh
kdb set user/tests/parent value
#> Create a new key user/tests/parent with string "value"
```

. The command above created a key `user/tests/parent` with the value `value`. Since Elektra uses a hierarchical database we can also create keys **below** `user/tests/parent`:

```sh
# If we do not provide a value, then `kdb set` creates keys with `null` values.
kdb set user/tests/parent/son
#> Create a new key user/tests/parent/son with null value

kdb set user/tests/parent/daughter
#> Create a new key user/tests/parent/daughter with null value

kdb set user/tests/parent/daughter/grandchild
#> Create a new key user/tests/parent/daughter/grandchild with null value
```

. We can check the hierarchy of the keys using `kdb ls` and retrieve data using `kdb get`:

```sh
# Elektra sorts keys alphabetically
kdb ls user/tests/parent
#> user/tests/parent
#> user/tests/parent/daughter
#> user/tests/parent/daughter/grandchild
#> user/tests/parent/son

kdb get user/tests/parent
#> value
kdb get user/tests/parent/daughter
#>
```

.

## Array Keys

Since Elektra sorts keys alphabetically we can use the key-value pair structure described above to store sequences of values (aka. arrays).

### Empty Arrays

For an **empty array** (`[]`) we just add the [metakey](../help/elektra-metadata.md) `array`:

```sh
# Create an empty array with the name `user/tests/sequence`
kdb meta-set user/tests/sequence array ''
```

.

### Array Elements

To create an **array element** we start the basename of a key with the `#` character and add the index of the array element afterwards. For example, the commands below adds three elements to our array `user/tests/sequence`:

```sh
kdb set user/tests/sequence/#0 'First Element'
kdb set user/tests/sequence/#1 'Second Element'
# Arrays do not need to be contiguous
kdb set user/tests/sequence/#3 'Fourth Element'
```

. As you can see above arrays can contain “empty fields”: The key `user/tests/sequence/#2` is missing.

For array elements with an index larger than `9` we must add **underscores** (`_`) to the basename, so we do not destroy the alphabetic order of the array. For example, to add a eleventh element to our array we use the following command:

```sh
kdb set user/tests/sequence/#_10 'Eleventh Element'
```

. The order of the array sequence is still correct afterwards, as the following command shows:

```sh
# List all array elements
kdb ls user/tests/sequence/
#> user/tests/sequence/#0
#> user/tests/sequence/#1
#> user/tests/sequence/#3
#> user/tests/sequence/#_10
```

. For larger indices we add **one underscore less, than the number of digits** of the index. For example, to add a element with index `1337` (`4` digits) we use the basename `#___1337`. We can also generate the basename programmatically:

```bash
ruby -e 'print("#", "_" * (ARGV[0].length - 1), ARGV[0])' 12345
#> #____12345

ruby -e 'print("#", "_" * (ARGV[0].length - 1), ARGV[0])' 0
#> #0

ruby -e 'print("#", "_" * (ARGV[0].length - 1), ARGV[0])' 42
#> #_42
```

.

### Metadata

To make life easier for users, Elektra’s [storage plugins](plugins.md) should save the [basename of the last key in the array parent](../decisions/array.md). Some plugins do not support this feature. In that case you can add this value manually via `kdb meta-set`:

```sh
# Add array elements
kdb set user/tests/favorites/superheros/#0 'One-Punch Man'
kdb set user/tests/favorites/superheros/#1 'Mermaid Man and Barnacle Boy'
kdb set user/tests/favorites/superheros/#____99999 'The guy with the bow and arrow'

# The metakey `array` should save the basename of the last element.
kdb meta-set user/tests/favorites/superheros array '#____99999'
```

. This way you can always retrieve the last element of an array easily:

```sh
kdb get user/tests/favorites/superheros/`kdb meta-get user/tests/favorites/superheros array`
#> The guy with the bow and arrow
```

.

# Closing Remarks

We close this tutorial by removing the data created by the commands above:

```sh
kdb rm -r user/tests
```

.
