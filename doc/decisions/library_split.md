# Library Split

## Problem

Only `libelektra-core` is supposed to access private data, but this contradicts the goal to keep the library minimal.
`kdbprivate.h` was too generic, it contained many other parts next to the struct definitions of Key/KeySet.

## Constraints

- The [C99 standard, section 5.2.4.1](http://www.open-std.org/jtc1/sc22/wg14/) gives following limit:
  4095 external identifiers in one translation unit
- Some parts of Elektra, like `mmapstorage` need access to private data structure.
- Elektra does not support several different struct definitions of Key/KeySet.
  Alternative implementations that want to coexist (e.g. `mmapstorage` should still work)
  must use the same struct definitions of Key/KeySet.

## Assumptions

## Considered Alternatives

- keep current situation

## Decision

Also allow `libelektra-extra` (and maybe other explicitly documented libraries) to access private Key/KeySet.
Put struct definitions of Key/KeySet in a separate header file, which gets included by parts that need it.

All currently planned libraries and their respective API prefixes are listed in the [Notes](#notes) below.

## Rationale

- allows various users (plugins, applications) to link to (more or less) exactly what they need
- allows symbol versioning on different levels (for different evolving libraries)
- allows alternative implementation of parts of Elektra, e.g. a libcore written in Rust
- facilitates code reuse between plugins

## Implications

- we need to clearly communicate which plugins must be exactly in the version of the `libelektra-core`
- all libraries will share a versioning scheme and are only supported if used in the same version

## Related Decisions

- [Lowlevel library](lowlevel_library.md)
- [Extra library](extra_library.md)

## Notes

- `libelektra-core`:
  The core minimal API of Elektra.
  Defines what `ElektraKey` and `ElektraKeyset` are, and contains the minimal API for manipulating them.

  **Prefixes:** `elektraKey*` and `elektraKeyset*`

- `libelektra-kdb`:
  The main API for interfacing with the KDB.

  **Prefix:** `elektraKdb*`

- `libelektra-lowlevel-c`:
  Additional C APIs that are useful when working with `ElektraKey` and `ElektraKeyset`, but not are not minimal API.
  Specifically targets C and not intended for use via bindings.

  **Prefixes:** `elektraCKey*`, `elektraCKeyset*`

- `libelektra-highlevel-c`:
  The C high-level API for reading/writing configuration with Elektra.
  Intended for use by applications.
  Partially intended for use with code-generation.
  Specifically targets C and not intended for use via bindings.

  **Prefix:** `elektraHlc*`
  (Note: Even though there isn't any other `highlevel` implementation, `c` suffix to show this lib is only for C)

- `libelektra-opts`:
  The API for parsing command-line arguments according to Elektra's spec.

  **Prefix:** `elektraOpts*`
- `libelektra-notification`:
  The API for setting up callbacks and automatically updated variables linked to changes in the KDB.

  **Prefix:** `elektraNotification*`

- `libelektra-io`:
  Elektra's API for asynchronous operations.

  **Prefix:** `elektraIo*`

- `libelektra-merge`:
  The API for merging and/or detecting conflicts between two keysets.

  **Prefix:** `elektraMerge*`

- `libelektra-plugin`:
  The plugin API.
  It forms the base for all plugins and contains the functions required to implement a plugin.
  It also contains the API for interacting with (other) plugins.
  This includes loading plugins and calling exported functions.

  Note: This library comes with two separate headers.
  One for implementing a plugin and another for loading/calling plugins.

  **Prefix:** `elektraPlugin*`
  (Note: previously `libelektra-plugin` merged with `libelektra-invoke`)

- `libelektra-pluginload`:
  Internal static library, linked into `libelektra-plugin` (re-exported) and `libelektra-kdb` (not exported).
  Contains the code for loading plugins.

  **Prefix:**: `elektraPlugin*` (shared with `libelektra-plugin` because exported there)

- `libelektra-mount`:
  The API for manipulating mountpoints.

  **Prefix:** `elektraMount*`

- `libelektra-type`:
  The API that defines Elektra's type system.

  **Names:** `elektra<TYPE>ToString` and `elektraKeyTo<TYPE>`
  (Note: extracted from `libelektra-ease`)

- `libelektra-extra`:
  Contains extra APIs for `ElektraKey` and `ElektraKeyset` beyond the minimal API of `libelektra-core`.
  These APIs are things that could be considered part of the classes for `ElektraKey` and `ElektraKeyset`, but which we do not consider minimal.
  The APIs should not specifically target C (see `libelektra-lowlevel-c` for that) and should be usable via bindings (if appropriate for the other language).

  **Prefix:** `elektraExtra*`
  (Note: includes `elektraExtraKeysetCut`, etc.; may also include stuff from old `libelektra-ease` or `libelektra-meta`)

- `libelektra-ease`:
  A collection of various other APIs that help when interacting with `ElektraKey` and `ElektraKeyset`.
  These APIs go beyond what could be considered part of the "classes" for `ElektraKey` and `ElektraKeyset` (e.g., SHA256 hashes).
  The APIs should not specifically target C (see `libelektra-lowlevel-c` for that) and should be usable via bindings (if appropriate for the other language).

  **Prefix:** `elektraEase*`
  (Note: rest of old `libelektra-ease`, merged with `libelektra-meta`; cleanup needed)

- `libelektra-utility`:
  Standalone helper functions that don't depend on `libelektra-core`

  **Prefix:** `elektraUtil*`

- `libelektra-base`:
  Internal static library, linked into `libelektra-core` (not exported) and `libelektra-utility` (partially re-exported).
  Contains helper functions for e.g., memory allocations, string formatting, etc.

  **Prefix:** `elektra*` (for internal), `elektraUtil*` (for exported)
