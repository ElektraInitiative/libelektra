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

  1.  Some parts are truly private, i.e. shouldn't be used outside the library that defines them.
      These things are only there because a library was split into multiple `.c` files.
      This includes e.g. `splitNew` and `struct _Split`.

      Symbols belonging to this category should not appear at all in the `symbols.map` file.

  2.  Other things are truly private, but must be tested.
      This includes e.g. most other `split*` functions or the `elektraKeyName*` functions.

      Symbols belonging to this category should not appear in at all in the `symbols.map` file.

  3.  Some things are not part of the public API and will never be part of the public API.
      This includes the `struct _Key` and `struct _KeySet`, but `elektraMalloc` and the high-level functions needed for codegen.
      These things will never be public API, because we don't want to make all the guarantees associated with that.
      Nonetheless, they cannot truly be private, because functions in other libraries need access.
      Each of these functions/structs/symbols has a specific "target audience" that needs access.

      Symbols belonging to this category should not appear in a public section of the `symbols.map` file.

  4.  Finally, there things that aren't part of the public API, but may be in future.
      This includes e.g. `ksFindHierarchy` or `elektraReadArrayNumber`.
      These functions could be public, but for various reasons are not.
      Maybe they are not well-tested, or maybe we just don't want to commit to the function yet.

      Symbols belonging to this category should not appear in a public section of the `symbols.map` file.

## Considered Alternatives

## Decision

A library `foo` may have these headers (covering categories 1, 3 & 4 from above):

- `src/lib/foo.h`:
  Contains the public API of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo.h`.
- `src/lib/foo/internal.h`:
  Contains the unstable API of `libelektra-foo`.
  Will be installed as `<include-root>/elektra/foo/internal.h`.
- `src/lib/foo/**/*.h`:
  Additional headers may be present.
  By default, they are private API and will not be installed.

  CMake can be used to install additional headers.
  Any of these installed headers `installed.h` must be included from `src/lib/foo/public.h`, or `src/lib/foo/internal.h` via a line `#include <elektra/foo/installed.h>`.
  This inclusion determines what kind of API the header contains.
  If `src/lib/foo/public.h` or `src/lib/foo/internal.h` include multiple of these extra headers, it must not declare any API itself.
  This is called a modularized library.
  Libraries that only install the default `src/lib/foo/public.h` or `src/lib/foo/internal.h` headers are called monolithic.

  The non-installed private headers must (of course) not be included in any of the installed headers (including the default ones).

Moving symbols between (non-private) headers is always a breaking change.

Plugins do not declare their API via header files.
Their headers are never installed and can be named and included any way the developer wants.

As seen above, all headers will be installed into a subfolder of `<include-root>/elektra`, where `<include-root>` is e.g. `/usr/include`.

For category 2 from above (private but needs to be tested), there are two options:

1. Declare functions as `static` in a `.c` file and `#include ""` this file from the test.
2. Add a private non-installed header that declares the API that needs testing, `#include ""` that and compile the test sources together with the `.o` files from the library (static linking).

If a symbol is needed in only one file and for tests, option 1 should be preferred.
For symbols that are used in multiple files, a header needs to exist anyway.
In any case, these unit tests should not be installed and should statically link the library into the test executable.
This way we don't pollute our `symbols.map` files and keep the number of exported symbols down.

## Rationale

- This structure makes the `#includes` simple and works nicely with our directory structure.
- Having a uniform naming convention simplifies things, both for developers writing Elektra and those using Elektra.
- Requiring libraries must either be fully modularized (main headers are only `#includes`) or completely monolithic, avoids the situation where a library has some stuff in extra headers, but sometimes you need the main `public.h`.
  This is bad, because it encourages users to simply include `public.h`, which defeats the point of modularized headers.

## Implications

- CMake must do validation of `#includes`. This can be done via simple `grep`.

## Related Decisions

- [Header Include](header_include.md)
- [Library Directory Structure](library_directory_structure.md)

## Notes

Initial text copied from @kodebach https://github.com/ElektraInitiative/libelektra/issues/4219
