# Capabilities

## Problem

Only plugins like `dump` and `quickdump` are able to represent any KeySet (as they are designed to do so).
Limitations of other storage plugins are e.g., that not every structure of configuration is allowed.

Some of these limitations were documented `infos/status`, others were not.

## Constraints

- tooling should be able to query which limitations exist

## Assumptions

## Considered Alternatives

- Implementing plugins that work around the limitations
  (e.g. escape the characters or rewrite directory values)
  is too complex and lead to new problems (e.g. escaping of
  the rewritten values and interactions of plugins, e.g.
  renaming and notification).
- Capabilities API was also found too complex, as application
  developers usually do not exactly know the requirements
  of their underlying format, especially if some parts
  of the configuration is extensible or derived from user-input.

## Decision

Add `infos/features/storage` to document limitations of storage plugins.
Ideally, storage plugins should throw an error in `kdbSet` for unrepresentable KeySets.

Elektra cannot guarantee that any configuration file format can be mounted anywhere.
Developers, maintainers and administrators are responsible for what they mount.
They need to test the setup.

## Rationale

## Implications

## Related Decisions

- [Base Name](../5_implemented/base_name.md)

## Notes

See also [#3504](https://issues.libelektra.org/3504):
