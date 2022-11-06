# Metadata in Spec Namespace

## Problem

To make storage-plugins suitable for `spec` they need to be able to store
all the metadata as specified in [METADATA.ini](/doc/METADATA.ini).
Most file formats do not have support for that.

If metadata is merged from different namespaces, e.g., `spec:` and `user:`,
metadata from one namespace might end up in keys of other namespaces, e.g.,
metadata from `spec:` might end up in `user:`.

## Constraints

- We do not touch what is already described in the
  [storage plugin tutorial](/doc/tutorials/storage-plugins.md).

## Assumptions

- Metadata can always safely merged from `spec:/` to other
  namespace.

## Considered Alternatives

- Store metadata in the comments like the `ini` plugin:
  this exposes internal metadata into the comments and
  can drastically affect the readability of a storage file.
  Comments should never be touched by a parser.
- Designate metadata to be only for `spec:/` or to only
  for restoring configuration file formats.
  Rename metadata `<type>` in `spec:/` to `check/<type>`
  or `make/array`.
  This way no merging of metadata would be needed and
  by the name alone it would be clear to which namespace
  it belongs.
- Do not support `array` or `type` if the underlying
  configuration file format does not support it.
  `spec-mount` could make sure that the storage plugin
  supports everything the underlying format needs.

## Decision

Do not store metadata unrelated to the configuration file structure in any namespace except in `spec:/`.

- Trying to store any other metadata in any other namespace leads to an error.
  E.g. `kdb set-meta user:/data metadata_not_suitable_for_storage_plugins something` would fail
  (validated by `spec` plugin).
- Metadata that is designed to be stored by storage plugins to preserve configuration file structure.
  E.g. `comment` or `order`, might be stored in any namespace.

Sometimes, the same metadata can be used in several namespaces but with different meanings
and ways of serialization, e.g. `type` and `array`:

- In `spec:/` the metadata `array=` (empty value) means "this is an array".
  If you give it a value e.g. `array=#4` it means "this is an array with default size X" (e.g. `#4` = size 5).
- In any other namespace `array=` means "this is an empty array" and e.g. `array=#4` means "this is an array with max index #4".
  `array=#4` is not stored literally but inferred.
- Either the storage plugin does not support arrays, then the metadata will be discarded
  on `kdbSet` but `spec` will keep on adding it for every `kdbGet`.
- Or, if the storage plugin supports arrays, the data will be serialized as array
  (even if the metadata comes from `spec`) and as such available in the next `kdbGet`
  from the storage plugin to be validated by `spec`.

Use different storage plugins, or plugins with different configurations,
for the `spec:/` namespace:

- `ni`
- TOML with `meta` configuration

The `kdb mount` tool will add the `meta` plugin configuration when mounting a storage plugin to `spec:/`.

## Rationale

- We do not need a storage plugin suitable for everything.
- The problems that internal metadata ends up in configuration files disappears.
- The short names `type` and `array` can be used in specs.
- Also simple storage plugins (like properties) can still be used with realistic
  specifications (that include array and/or type).

## Implications

- We need to have different (default) plugins in `spec:/` than in the other namespaces.
- It can be confusing about which metadata we are speaking, e.g., `array` in
  `spec:/` has a quite different meaning to `array` in other namespaces.
- The spec:/ meaning of array= must obviously be stored. The other meaning should never
  be stored explicitly and instead should be inferred from the stored data. Either by
  the storage plugin (e.g. JSON array) or by the spec plugin.

## Related Decisions

- [Spec Expressiveness](spec_expressiveness.md)
- [Arrays](../4_partially_implemented/array.md)

## Notes
