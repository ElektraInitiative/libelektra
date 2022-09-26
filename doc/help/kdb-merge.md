# kdb-merge(1) -- Three-way merge of KeySets

## NAME

kdb-merge - Join three key sets together

## SYNOPSIS

`kdb merge [OPTIONS] our their base result`

- ourpath:
  Path to the keyset to serve as `our`<br>

- theirpath:
  path to the keyset to serve as `their`<br>

- basepath:
  path to the `base` keyset<br>

- resultpath:
  path without keys where the merged keyset will be saved<br>

## DESCRIPTION

`kdb merge` can incorporate changes from two modified versions (our and their) into a common preceding version (base) of a key set. This lets you merge the sets of changes represented by the two newer key sets. This is called a three-way merge between key sets.<br>
On success the resulting keyset will be saved to mergepath.<br>
On unresolved conflicts nothing will be changed.<br>

## OPTIONS

The options of `kdb merge` are:

- `-f`, `--force`: overwrite existing keys in `result`
- `-v`, `--verbose`: give additional information

Strategies offer fine grained control over conflict handling. The option is:

- `-s <name>`, `--strategy <name>`: which is used to specify a strategy to use in case of a conflict

Strategies have their own [man page](/doc/help/elektra-merge-strategy.md) which can be accessed with `man elektra-merge-strategies`.

## RETURN VALUE

- 0:
  Successful.

- 11:
  An error happened.

- 12:
  A merge conflict occurred and merge strategy abort was set.

The result of the merge is stored in `result`.

## THREE-WAY MERGE

You can think of the three-way merge as subtracting base from their and adding the result to our, or as merging into our the changes that would turn base into their. Thus, it behaves exactly as the GNU diff3 tool.
These three versions of the KeySet are:<br>

- `base`:
  The `base` KeySet is the original version of the key set.<br>

- `our`:
  The `our` KeySet represents the user's current version of the KeySet.<br>
  This KeySet differs from `base` for every key you changed.<br>

- `their`:
  The `their` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).<br>
  This KeySet differs from `base` for every key someone has changed.<br>

The three-way merge works by comparing the `our` KeySet and the `their` KeySet to the `base` KeySet. By looking for differences in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.<br>

## CONFLICTS

Conflicts occur when a key has a different value in all three key sets or when only base differs. When all three values for a key differ, we call this an overlap. Different [merge strategies](elektra-merge-strategy.md) exist to resolve those conflicts.<br>

## EXAMPLES

To complete a simple merge of three KeySets:<br>

````sh
kdb set user:/base "A"
#> Create a new key user:/base with string "A"
kdb set user:/their "A"
#> Create a new key user:/their with string "A"
kdb set user:/our "B"
#> Create a new key user:/our with string "B"
kdb merge user:/our user:/their user:/base user:/result
kdb get user:/result
#>B

```<br>

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
````
