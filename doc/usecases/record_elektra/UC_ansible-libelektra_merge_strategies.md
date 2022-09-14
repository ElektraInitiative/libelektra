# Use Case: Select merge strategy in ansible-libelektra

## Summary

- **Title:** Select merge-strategy in ansible-libelektra
- **Scope:** ansible-libelektra
- **Level:** User goal
- **Actors:** User (usually sysadmin)
- **Brief:** It should be possible to specify the merge strategy used, e.g. OURS, THEIRS, ABORT. ansible-libelektra should then use this strategy when merging.

## Scenarios

- **Precondition:** -
- **Main success scenario:** If the user has specified a strategy, this strategy is applied on merge conflicts.
- **Alternative scenario:** If no strategy is specified, a sane (and documented) default strategy should be used.
- **Error scenario:** If there are still conflicts that could not be resolved, the offending keys should be reported.
- **Postcondition:** -
- **Non-functional Constraints:** -
