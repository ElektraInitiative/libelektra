# Specification

## Issue

- The commandline for mounting a backend can be very long
- A specification is already used for code generation, but
 - the data is not available in KDB
 - the specification cannot be used for mounting, even though the
   needed information is in the specification

## Constraints

- the specification must be simple to use
- there must be a clear mapping between keys and configuration file
  entries
- there must be a 1:1 mapping of keys used within an application and the
  specification

## Assumptions

- The user will have a variable for each configuration value
  (e.g. by using a binding) if she cares about access performance
- memory footprint is expected to be low

## Considered Alternatives

- modifying kdbGet() and ksLookup() so that we get a logical hierarchy
- having a duplicated in memory hierarchy with the cascaded key sets

## Decision

## Argument

- The user wants deterministic retrieval of configuration.
  It must be answerable (w/o study of source code and debugging) which
  value will be used in the current situation.

## Implications

1. Introduction of spec/ hierarchy
2. kdbGet() for cascading
3. kdbSet() for cascading
4. modify ksLookup() for cascading
5. KDB_O_DEFAULT in ksLookup()
6. define necessary metadata
7. mounting spec files

## Related decisions

## Notes
