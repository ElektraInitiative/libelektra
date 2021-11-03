# kdb-gen-highlevel(1) -- High-level API code-generator template

## SYNOPSIS

`kdb gen highlevel <parentKey> <outputName> [parameters...]`

- `<parentKey>`:
  the parent key to use, MUST be in the `spec` namespace
- `<outputName>`:
  the base name of the output files. `.c` will be appended for the source file and `.h` for the header file.
- `[parameters...]`:
  see [below](#parameters)

## DESCRIPTION

This command invokes the code-generator with the template for the high-level API.

The input for this template is a specification. Keys below the `parentKey` which have a `type` metakey are considered
part of this specification. Every such key must also either have a `default` metadata or alternatively must be marked
with the `require` metakey. Keys marked with `require` must be set by the user, otherwise the initialization of the
Elektra handle will fail.

The `type` metakey may only have one of the following values: `enum`, `string`, `boolean`, `char`, `octet`, `short`,
`unsigned_short`, `long`, `unsigned_long`, `long_long`, `unsigned_long_long`, `float`, `double`, `long_double`,
`struct`, `struct_ref` and `discriminator`. Most of these values correspond to the values supported by the high-level API,
the remaining values (`enum`, `struct`, `struct_ref`, `discriminator`) are treated specially and are part of advanced concepts.
For more information on these concepts take a look at [elektra-highlevel-gen(7)](elektra-highlevel-gen.md). If one of the
advanced `type` values is used, you should also set `check/type = any`; otherwise the `type` plugin may produce an error.

The template produces at least three output files: `<outputName>.c`, `<outputName>.h` and `<outputName>.mount.sh`.
The `.c` file only contains implementations, therefore its precise content will not be documented.

The header (`.h`) file contains the following parts:

1. Generated `enum`s and `struct`s
2. Declarations for generated accessor functions
3. Tags for accessing keys
4. `static inline` accessor functions for all tags
5. Declarations of initialization functions
6. Convenience accessor macros

Anything else that may be part of the header file is not considered public API and may be subject to change at any point in time.
There is also no guarantee of full compatibility between Elektra version for the documented parts of the header, however,
we try to have as little breaking changes as possible for the six parts listed above.

The `.mount.sh` file is a shell script that can be used to mount the specification for the application. You can either
use it as a basis for your own script, or wrap it in another script that correctly sets `APP_PATH` or `SPEC_FILE`
(depending on your configuration). If the generated script happens to use the correct paths already, you can of course
use it directly as well.

For detailed information about the contents of the header file see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md).

## PARAMETERS

- `initFn`:
  Changes the name of the initialization function (default: `loadConfiguration`)
- `helpFn`:
  Changes the name of the function that prints the generated help message (default: `printHelpMessage`)
- `specloadFn`:
  Changes the name of the function that checks for "specload mode" (default: `exitForSpecload`)
- `tagPrefix`:
  Changes the prefix of the generated tags (default: `ELEKTRA_TAG_`)
- `embedHelpFallback`:
  Switches whether a fallback help message should be embedded; allowed values: `1` (default), `0`
  If enabled (`1`), a help message will be generated from the specification passed to the code-generator and embedded
  into the application. This message will be used, if the normal help message could not be generated at runtime.
- `enumConv`:
  Switches how enum conversion should be done; allowed values: `default` (default), `switch`, `strcmp`
  - `strcmp`: uses a simple series of `if (strcmp(*, *) == 0)` to convert strings into enums
  - `switch`: constructs a series of `switch` statements to convert strings into enums
  - `auto`: uses a `switch` up to a depth of 2, then switches to `strcmp`
- `headers`:
  Comma-separated (`,`) list of additional header files to include. For each of the listed headers we will generate an `#include "*"`
  statement
- `genSetters`:
  Switches whether setters should be generated at all; allowed values: `1` (default), `0`
- `embeddedSpec`:
  Changes how much of the specification is embedded into the application; allowed values: `full` (default), `defaults`, `none`.
  see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)

## EXAMPLES

The simplest invocation is:

`kdb gen highlevel /sw/org/app/#0/current config`

However, it is not recommended having the code-generator read from the KDB, so one should instead use:

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config`

If you don't want to embed the full specification in your binary, we recommend:

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config embeddedSpec=defaults`

For the minimal binary size you may use (this comes with its own drawbacks, see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)):

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config embeddedSpec=none`

## SEE ALSO

- [kdb(1)](kdb.md) for general information about the `kdb` tool.
- [elektra-spec(7)](elektra-spec.md) for an explanation of the `spec` namespace.
