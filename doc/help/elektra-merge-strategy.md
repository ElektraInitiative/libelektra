# elektra-merge-strategies(7) -- how to merge key sets

Elektra's merge library offers different strategies to resolve conflicts in merges without user interaction.
Consequently, Elektra's tools all have access to the following merge strategies:

- `abort`: the merge will abort if any conflict happens and merge the 3 key sets together otherwise.
- `our`: This option forces conflicting keys to be auto-resolved cleanly by favoring `our`. Changes from the other key sets that do not conflict with the `our` version are reflected in the merge result. This works like the recursive merge strategy with the `ours` option of git.
- `their`: This is the opposite of `our`. The merge will use the `their` version when a conflict happens.

If no strategy is specified, the merge will default to the abort strategy.
