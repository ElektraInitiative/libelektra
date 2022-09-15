# Use Case: Select merge strategy in ansible-libelektra

## Summary

- **Title:** Select merge-strategy in ansible-libelektra
- **Scope:** ansible-libelektra
- **Level:** User goal
- **Actors:** User (usually sysadmin)
- **Brief:** It should be possible to specify the merge strategy used for conflicts. The merging should be performed by ansible-libelektra. 
  The following strategies should be implemented:
    - OURS: use the new values
    - THEIRS: keep the old values
    - ABORT: on conflict don't merge and report an error
  

## Scenarios

- **Precondition:** -
- **Main success scenario:** If the user has specified a strategy, this strategy is applied on merge conflicts.
- **Alternative scenario:** If no strategy is specified OURS is used.
- **Error scenario:** If there are still conflicts that could not be resolved, the offending keys should be reported.
- **Postcondition:** -
- **Non-functional Constraints:** -
