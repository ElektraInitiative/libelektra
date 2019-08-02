# kdb-cmerge(1) -- Three-way merge of KeySets

## SYNOPSIS

`kdb cmerge [options] ourpath theirpath basepath resultpath`<br>

- ourpath:
  Path to the keyset to serve as `ours`<br>

- theirpath:
  path to the keyset to serve as `theirs`<br>

- basepath:
  path to the `base` keyset<br>

- resultpath:
  path without keys where the merged keyset will be saved<br>

## DESCRIPTION

Does a three-way merge between keysets.<br>
On success the resulting keyset will be saved to mergepath.<br>
On unresolved conflicts nothing will be changed.<br>

## THREE-WAY MERGE

The `kdb cmerge` command uses a three-way merge by default.<br>
A three-way merge is when three versions of a file (or in this case, KeySet) are compared in order to automatically merge the changes made to the KeySet over time.<br>
These three versions of the KeySet are:<br>

- `base`:
  The `base` KeySet is the original version of the KeySet.<br>

- `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.<br>
  This KeySet differs from `base` for every key you changed.<br>

- `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).<br>
  This KeySet differs from `base` for every key someone has changed.<br>

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.<br>

## CONFLICTS

Conflicts occur when a Key has a different value in all three KeySets.<br>

## EXAMPLES

To complete a simple merge of three KeySets:<br>
`kdb cmerge user/our user/their user/base user/result`<br>

## SEE ALSO

- [elektra-cmerge-strategy(7)](elektra-cmerge-strategy.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
