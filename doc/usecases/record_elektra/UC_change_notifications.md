# Use Case: Notify plugins when data in the KDB changes

## Summary

- **Title:** Notify plugins when data in the KDB changes
- **Scope:** Configuration/Logging
- **Level:** Developer goal
- **Actors:** Plugin developer
- **Brief:** Some plugins are only executed when something changed in the KDB and as an input receive the keys that changed in their old and new state.

## Scenarios

- **Precondition:**
  - Something in the KDB changed
  - A plugin listening to such a hook is enabled
- **Main success scenario:**
  - After a change in the KDB the plugin(s) listening on the hooks are notified
  - The plugins receive both the old and new state of the keys
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  - Determining which plugin(s) listen on this hook should not noticably slow down Elektra.
