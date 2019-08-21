# elektra-cmerge-strategies(7) -- how to merge key sets

Elektra's cmerge tool tries to avoid conflicts using semantic aspects of configuration files. Those configuration files are represented as key sets in Elektra. Those are collections of name-value pairs.
The three-way merge works by comparing the `our` key set and the `their` key set to the `base` key set. By looking for differences in these key sets, a new key set called `result` is created. This result represents a merge of these key sets.
In case a conflict is unavoidable, different predefined merge strategies can be used to resolve them without user interaction.

These strategies will supersede the old [merge strategies](elektra-merge-strategies.md) once `kdb cmerge` supersedes `kdb merge`.

## 3-WAY

A common scenario to use Elektra's cmerge is a package upgrade. Terminology is oriented to this scenario.

- `base`:
  The `base` key set is the original version of the key set before the package upgrade.

- `our`:
  The `our` key set represents the user's current version of the key set.
  This key set differs from `base` for every key you changed.

- `their`:
  The `their` key set is the original version of the key set after the package upgrade. In a broader context, this version key set differs from `base` for every key somebody else changed.

## STRATEGIES

The following strategies exist:

- abort:
  Abort if any conflict occurs. This is the default strategy.

- our:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring our version. Changes from base or their that do not conflict with our side are reflected to the merge result.

- their:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring their version. Changes from our or base that do not conflict with their side are reflected to the merge result.

- base:
  This option forces conflicting hunks to be auto-resolved cleanly by favoring base version. Changes from our or their that do not conflict with base side are reflected to the merge result.
