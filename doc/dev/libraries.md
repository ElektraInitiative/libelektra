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

  **Prefix:** `elektra*`

- `libelektra-highlevel-c`:
  The C high-level API for reading/writing configuration with Elektra.
  Intended for use by applications.
  Partially intended for use with code-generation.
  Specifically targets C and not intended for use via bindings.

  **Prefix:** `elektraHlc*` (better suggestions welcome)
  (Note: Even though there isn't any other `highlevel` implementation, I added the `-c` suffix to show this lib is only for C)

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

  **Prefix:** `elektraMerge`

- `libelektra-pluginbase`:
  The API _used by_ plugins.

  **Prefix:** `elektraPlugin*`
  (Note: previously `libelektra-plugin`)

- `libelektra-plugin`:
  The API for _interacting with_ plugins.
  This includes loading plugins and calling exported functions.

  **Prefix:**: `elektraPlugin*`
  (Note: previously `libelektra-invoke` & `loader`)

- `libelektra-mount`:
  The API for manipulating mountpoints.

  **Prefix:** `elektraMount*`

- `libelektra-type`:
  The API that defines Elektra's type system. (Alternative: The API that defines the type system used by `libelektra-highlevel-c`.)

  **Names:** `elektra<TYPE>ToString` and `elektraKeyTo<TYPE>`
  (Note: extracted from `libelektra-ease`)

- `libelektra-ease`:
  A collection of various other APIs that help when interacting with `ElektraKey` and `ElektraKeyset`.
  Contains APIs that are not specific enough to justify a separate library and not minimal enough for `libelektra-core`, but can be implemented on top of the public API.
  The APIs should not specifically target C and should be usable via bindings (if appropriate for the other language).

  **Prefix:** `elektra*`
  (Note: rest of old `libelektra-ease`, merged with `libelektra-meta` & `libelektra-utility`; cleanup needed)

- `libelektra-operations`:
  Like `libelektra-ease`, this library contains various additional `ElektraKey` and `ElektraKeyset` APIs beyond the minimal API.
  However, these APIs may need access to private APIs of `libelektra-core` to be implemented (efficiently).
  The APIs should not specifically target C and should be usable via bindings (if appropriate for the other language).
  All APIs must abstract over unstable details of private APIs and provide a stable API, or **very clearly** state the stability restrictions.

  Importantly, the distinction between `libelektra-ease` and `libelektra-operations` **is not** whether access to private APIs is needed.
  `libelektra-operations` is intended for use by most applications, plugins, tools, etc. while `libelektra-ease` should be used less often and serves more niche use cases.
  For example: `elektraKeysetCut`/`elektraKeysetFindHierarchy` from `libelektra-operations` can be useful in many plugins and applications.
  `elektraKeysetSortTopological` from `libelektra-ease` on the other hand serves a very niche use case.

  **Prefix:** `elektra*`
  (Note: includes `elektraKeysetCut`, etc.; may also include stuff from old `libelektra-ease` or `libelektra-meta`)
