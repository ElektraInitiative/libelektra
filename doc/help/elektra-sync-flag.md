# elektra-sync-flag(7) -- detection for writing

Elektra keeps track of changes the user makes on data structures.
Because the `KeySet` can be composed in the way the user wants it, the
`KeySet` does not give enough information if changes occurred within.
Instead, changes will be marked in individual `Key` objects. On modifying
functions, the so-called **sync flag** will be set.

See also `keyNeedSync()`.
