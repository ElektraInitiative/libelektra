# Including Headers

## Problem

In C you can include a header file with

```c
#include "header.h"
```

or

```c
#include <header.h>
```

In the C standard, section 6.10.2, paragraphs 2 to 4 state:

> 2. A preprocessing directive of the form
>
>    ```c
>    #include <h-char-sequence> new-line
>    ```
>
>    searches a sequence of implementation-defined places for a header identified uniquely by the specified sequence between the `<` and `>` delimiters, and causes the replacement of that directive by the entire contents of the header. How the places are specified or the header identified is implementation-defined.
>
> 3. A preprocessing directive of the form
>
>    ```c
>    #include "q-char-sequence" new-line
>    ```
>
>    causes the replacement of that directive by the entire contents of the source file identified by the specified sequence between the `"` delimiters. The named source file is searched for in an implementation-defined manner. If this search is not supported, or if the search fails, the directive is reprocessed as if it read
>
>    ```c
>    #include <h-char-sequence> new-line
>    ```
>
>    with the identical contained sequence (including `>` characters, if any) from the original directive.
>
> 4. A preprocessing directive of the form
>
>    ```c
>    #include pp-tokens new-line
>    ```
>
>    (that does not match one of the two previous forms) is permitted. The preprocessing tokens after include in the directive are processed just as in normal text. (Each identifier currently defined as a macro name is replaced by its replacement list of preprocessing tokens.) The directive resulting after all replacements shall match one of the two previous forms. The method by which a sequence of preprocessing tokens between a `<` and a `>` preprocessing token pair or a pair of `"` characters is combined into a single header name preprocessing token is implementation-defined.
>
> ### Definitions:
>
> - h-char: any member of the source character set except the new-line character and `>`
> - q-char: any member of the source character set except the new-line character and `"`

In short, the whole search process of the `#include` mechansim is implementation-defined.
However, because `#include` works (mostly) like a literal copy-paste, deciding between `""` and `<>` within a public header file can affect user code.

## Assumptions

- Modern Compilers work like [GCC](https://gcc.gnu.org/onlinedocs/cpp/Search-Path.html):
  - `#include "[header]"` treats `[header]` as file path relative to the current file (as defined by the standard, if such a file doesn't exist there is a fallback to the `<>` behavior)
  - `#include <[header]>` treats `[header]` as file path relative to one of the pre-defined include-paths

## Considered Alternatives

- Let developers decide:

  This would just create inconsistent code.

- Put all headers into one directory and use the same relative layout as will be installed. Then use `""`:

  This makes for an inconvenient development experience. It is also pretty hard to achieve for plugins and private headers would clutter the include-directory.

## Decision

The rules for including headers are:

- To include a private non-installed header (i.e., a file that is only available in the source repo) use:
  ```c
  #include "./header.h"
  // or
  #include "./subdir/header.h"
  ```
  It should not be necessary, to include a private non-installed header from another directory.
- Installed headers are included with their full path as if they are installed already:
  ```c
  #include <elektra/somelib.h>
  // or
  #include <elektra/somelib/header.h>
  ```
- System headers or headers for external libraries are included with:
  ```c
  #include <stdlib.h>
  #include <dbus/dbus.h>
  // etc.
  ```

We will set up the `build/include` directory to mirror the installed layout of header files.
This makes the `#include <>`s work correctly, without having to add any extra include paths except `build/include`.

We will also enforce that the path in a `#include ""` always starts with a `./` and does not contain any `/../`.
To do this, we will use a simple `grep` based script that runs as a test case and as an early part of the CI (like e.g., the formatting check).

The rules for `#include ""` do not apply to tests.
Tests can include anything from anywhere within the code base to allow testing private APIs.

## Rationale

The decision highlights the difference between installed and non-installed headers.
The main driving factor for using `""` at all was that including a non-installed private header with `<>` would be unexpected, since non-installed headers shouldn't be in the (standard) include-path.

See also considered alternatives.

## Implications

- All installed headers must be put into `build/include`.
- Non-installed headers meanwhile must be kept next to the `.c` files that use them (e.g. `src/libs/mylib`).

## Related Decisions

- [Header File Structure](header_file_structure.md)
- [Library Directory Structure](library_directory_structure.md)

## Notes
