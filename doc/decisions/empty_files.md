# Empty Files

## Issue

An empty KeySet is passed to kdbSet(). What is the correct persistent
representation?

## Constraints

## Assumptions

## Considered Alternatives

- no file, no empty directories
- keep directories, remove configuration file
- plugins write minimal, syntactical-valid configuration file
- plugins do whatever they think is correct
- remember initial situation at mounting time and restore it when empty
    key is passed (seems inefficient and complicated?)

## Decision

## Argument

## Implications

When something is written, a large grave of configuration files comes up
over time.

## Related decisions

## Notes
