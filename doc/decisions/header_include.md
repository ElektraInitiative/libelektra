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

- Always use `<>`:

  This can make things more difficult than necessary. We wouldn't utilize the common "relative path" behaviour. Therefore, user code must have all the same directories in the include-paths as our code (translated to their installed counterparts).

- Put all headers into one directory and use the same relative layout as will be installed. Then use `""`:

  This makes for an inconvenient development experience. It is also pretty hard to achieve for plugins and private headers would clutter the include directory.

## Decision

- Utilize CMake to make the following rules work.
- Including another header from the current directory is done with:
  ```c
  #include "header.h"
  ```
- Including another header from a subdirectory of the current one (e.g. from a third-party library whose code was copied into the repo) is done with:
  ```c
  #include "subdir/header.h"
  ```
- Including a header from another library within the Elektra codebase, is done with:
  ```c
  #include "../otherlib/header.h"
  ```
- Including Elektra library headers from a plugin, is done with:
  ```c
  #include <elektra/thelib/header.h>
  ```
  In other words, plugins are treated as external and must use the same rules an application would also use.
- Only including system headers and headers from external third-party libraries is done with:
  ```c
  #include <external.h>
  ```

## Rationale

See also considered alternatives.

## Implications

- All libraries and plugins keep their own headers next to the `.c` files in one directory per library (see also [Library Directory Structure](library_directory_structure.md)).
- We must set up CMake such that the relative includes work as expected within the build directory.

## Related Decisions

- [Header File Structure](header_file_structure.md)
- [Library Directory Structure](library_directory_structure.md)

## Notes
