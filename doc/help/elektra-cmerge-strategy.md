# elektra-cmerge-strategies(7) -- how to merge key sets

## 3-WAY

- `base`:
  The `base` KeySet is the original version of the KeySet.

- `ours`:
  The `ours` KeySet represents the user's current version of the KeySet.
  This KeySet differs from `base` for every key you changed.

- `theirs`:
  The `theirs` KeySet usually represents the default version of a KeySet (usually the package maintainer's version).
  This KeySet differs from `base` for every key someone has changed.

The three-way merge works by comparing the `ours` KeySet and the `theirs` KeySet to the `base` KeySet. By looking for differences in these KeySets, a new KeySet called `result` that represents a merge of these KeySets is created.

## STRATEGIES

The following strategies are planned to be implemented:

- abort:
  Abort if any conflict occurs.

- our:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring our version. Changes from base or their that do not conflict with our side are reflected to the merge result.

- their:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring their version. Changes from our or base that do not conflict with their side are reflected to the merge result.

- base:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring base version. Changes from our or their that do not conflict with base side are reflected to the merge result.
