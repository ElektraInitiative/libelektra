# elektra-merge-strategies(7) -- how to merge key sets

In elektra-tools a three-way merging was implemented.
It can also use be used for two-way merging, e.g.
for importing.

Note: For a two-way merge, the `ours` version of the keys is used
in place of `base`

## 3-WAY

- `base`:
  The `base` KeySet is the original version of the KeySet.

- `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.
  This KeySet differs from `base` for every key you changed.

- `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).
  This KeySet differs from `base` for every key someone has changed.

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences in these KeySets, a new KeySet called `result` is created that represents a merge of these KeySets.

## STRATEGIES

Currently the following strategies exist:

- preserve:
  Automerge only those keys where just one side deviates from base (default).

- ours:
  Whenever a conflict exists, use our version.

- theirs:
  Whenever a conflict exists, use their version.

- cut:
  Removes existing keys below the resultpath and replaces them with the merged keyset.

- unchanged: (EXPERIMENTAL, only for kdb-mount)
  Do not fail if the operation does not change anything.

- import: (DEPRECATED, avoid using it)
  Preserves existing keys in the resultpath if they do not exist in the merged keyset.
  If the key does exist in the merged keyset, it will be overwritten.
