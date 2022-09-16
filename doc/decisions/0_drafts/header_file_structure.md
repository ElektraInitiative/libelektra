# Header File Structure

## Problem

kdb.h contains the public API for both libelektra-core and libelektra-kdb.
It is confusing and makes it hard to see what library a function actually belongs to.

The big problem is kdbprivate.h. It has two main problems:

1. It contains stuff from many different libraries. I found at least libelektra-core, libelektra-kdb and libelektra-highlevel.
2. It contains things that are different levels of "private".

## Constraints

- Each library should have at least one separate header file:

  Without this constraint, we would constantly include things that aren't needed.
  This is inconvenient as it clutters the auto-suggestions of IDEs.
  It also slows down the compile process, since the compiler has to parse everything that was included (see [also](https://lore.kernel.org/lkml/YdIfz+LMewetSaEB@gmail.com/T/)).

- Non-public API and public API should be kept in separate files
- Some non-public APIs need to be accessible for testing, such headers should not be packaged/installed

## Assumptions

- There are different categories of "private":

  1.  Some parts are truly private, i.e., shouldn't be used outside the library that defines them.
      These things are only there because a library was split into multiple `.c` files.
      This includes e.g., `splitNew` and `struct _Split`.

      Symbols belonging to this category should not appear at all in the `symbols.map` file.

  2.  Other things are truly private, but must be tested.
      This includes e.g., most other `split*` functions or the `elektraKeyName*` functions.

      Symbols belonging to this category should not appear in at all in the `symbols.map` file.

  3.  Some things are not part of the public API and will never be part of the public API.
      This includes the `struct _Key` and `struct _KeySet`, but `elektraMalloc` and the high-level functions needed for codegen.
      These things will never be public API, because we don't want to make all the guarantees associated with that.
      Nonetheless, they cannot truly be private, because functions in other libraries need access.
      Each of these functions/structs/symbols has a specific "target audience" that needs access.

      Symbols belonging to this category should not appear in a public section of the `symbols.map` file.

  4.  Finally, there things that aren't part of the public API, but may be in future.
      This includes e.g., `ksFindHierarchy` or `elektraReadArrayNumber`.
      These functions could be public, but for various reasons are not.
      Maybe they are not well-tested, or maybe we just don't want to commit to the function yet.

      Symbols belonging to this category should not appear in a public section of the `symbols.map` file.

## Considered Alternatives

## Decision

### Libraries

A library can be monolithic or modularized.
Monolithic libraries should be small and bigger libraries with large APIs should be modularized.

A monolithic library `foo` may have these headers (covering categories 3 & 4 from above):

- `src/include/elektra/foo.h`:
  Contains the full public API of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo.h`.
- `src/include/elektra/foo/internal.h`:
  Contains the internal (i.e., non-stable) API of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo/internal.h`.

A modularized library `foo` may have these headers (covering categories 3 & 4 from above):

- `src/include/elektra/foo/public.h`:
  Declares the public API of `libelektra-foo`, by including `#include <elektra/foo/*.h>`.
  Will be installed as `<include-root>/elektra/foo/public.h`.

  Such a header may only include `#include <elektra/foo/*.h>` lines.

- `src/include/elektra/foo/*.h`:
  Additional public API header of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo/*.h`.

  All of these installed headers `installed.h` must be included from `src/lib/foo/public.h` via a line `#include <elektra/foo/installed.h>`.

- `src/include/elektra/foo/internal.h`:
  Declares the internal (i.e., non-stable) API of `libelektra-foo`, by including `#include <elektra/foo/*.h>`.
  Will be installed as `<include-root>/elektra/internal/foo.h`.

  Such a header may only include `#include <elektra/foo/*.h>` lines.

- `src/include/elektra/foo/internal/*.h`:
  Additional header of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo/internal/*.h`.

  All of these installed headers `installed.h` must be included from `src/lib/foo/internal.h` via a line `#include <elektra/foo/internal/installed.h>`.

Additionally, all libraries may also have private headers:

- `src/lib/foo/**/*.h`:
  Additional headers may be present.
  These headers may only be used by other files within `src/lib/foo`, according to the rules in [Including Headers](header_include.md).

### Plugins

Plugins do not declare their API via header files.
Their headers are never installed and can be named any way the developer wants.

### Tests

For category 2 from above (private but needs to be tested), one of the following should be done:

1. Declare functions as `static` in a `.c` file and `#include ""` this file from the test.
2. Add a private non-installed header (e.g. `src/lib/foo/testapi.h`) that declares the API that needs testing, `#include ""` that and compile the test sources together with the `.o` files from the library (static linking).

If a symbol is needed in only one file and for tests, option 1 should be preferred.
For symbols that are used in multiple files, a header needs to exist anyway.
In any case, these unit tests should not be installed and should statically link the library into the test executable.
This way we don't pollute our `symbols.map` files and keep the number of exported symbols down.

## Rationale

- This structure makes the `#include`s simple and works nicely with our directory structure.
- Having a uniform naming convention simplifies things, both for developers writing Elektra and those using Elektra.
- Requiring libraries must either be fully modularized (main headers are only `#includes`) or completely monolithic, avoids the situation where a library has some parts in `public.h` and other parts in extra headers.
  This is bad, because it encourages users to simply include `public.h`, which defeats the point of modularized headers.

## Implications

- The location of a header file within the source tree determines what API it contains (public, internal, or private).
- Some libraries are currently neither fully modularized nor fully monolithic.
  The headers for these libraries must be restructured.

## Related Decisions

- [Including Headers](header_include.md)
- [Library Directory Structure](library_directory_structure.md)

## Notes
