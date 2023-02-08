# Empty Files

## Problem

An empty KeySet is passed to kdbSet().
What is the correct persistent representation?

## Constraints

## Assumptions

- User does not want empty files lying around everywhere.
- User wants to come back to a clean situation using Elektra

## Considered Alternatives

- no file, no empty directories
- keep directories, remove configuration file
- plugins write minimal, syntactical-valid configuration file
- plugins do whatever they think is correct
- remember initial situation at mounting time and restore it when empty key is passed (seems inefficient and complicated?)

## Decision

Remove files on empty KeySet.

## Rationale

- allows user to undo what a previous kdbSet() did
- easy to understand semantics
- makes storage plugins easier (do not need to remove files)

## Implications

- less empty files are left
- no invalid empty files (yajl bugs)

## Related Decisions

## Notes
