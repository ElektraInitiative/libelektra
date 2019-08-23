# kdb-gen-highlevel(1) -- High-level API code-generator template

## SYNOPSIS

`kdb gen [options...] highlevel <parentKey> <outputName> [parameters...]`

- `[options...]`:
  see [kdb-gen(1)](kdb-gen.md)
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

The template produces two output files: `<outputName>.c` and `<outputName>.h`. The `.c` file only contains implementations,
therefore its precise content will not be documented.

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

For detailed information about the contents of the header file see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md).

## PARAMETERS

- `initFn`:
  Changes the name of the initialization function (default: `loadConfiguration`)
- `helpFn`:
  Changes the name of the function that prints the generated help message (default: `printHelpMessage`)
- `specloadFn`:
  Changes the name of the function that checks for "specload mode" (default: `specloadCheck`)
- `tagPrefix`:
  Changes the prefix of the generated tags (default: `ELEKTRA_TAG_`)
- `enumConv`:
  Switches how enum conversion should be done; allowed values: `default` (default), `trie`, `strcmp`
  - `strcmp`: uses a simple series of `if (strcmp(*, *) == 0)` to convert strings into enums
  - `trie`: constructs a character based trie to convert strings into enums
  - `default`: uses a `trie` up to a depth of 2, then switches to `strcmp`
- `headers`:
  Comma-separated (`,`) list of additional header files to include. For each of the listed headers we will generate an `#include "*"`
  statement
- `genSetters`:
  Switches whether setters should be generated at all; allowed values: `true` (default), `false`
- `specLocation`:
  Changes where the processed specification will be stored; allowed values: `embedded` (default), `external`.
  see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)
- `defaultsHandling`:
  Changes how the default values are embedded into the application; allowed values: `embedded` (default), `speconly`.
  see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)
- `specValidation`:
  Changes how the specification will be validated on application start-up; allowed values: `none` (default), `minimal`.
  see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)

## EXAMPLES

The simplest invocation is:

`kdb gen highlevel /sw/org/app/#0/current config`

However, it is not recommended to have the code-generator read from the KDB, so one should instead use:

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config`

If you don't want to embed the full specification in your binary, we recommend:

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config specLocation=external specValidation=minimal`

For the minimal binary size you may use (this comes with its own drawbacks, see [elektra-highlevel-gen(7)](elektra-highlevel-gen.md)):

`kdb gen -F ni=spec.ini highlevel /sw/org/app/#0/current config specLocation=external defaultsHandling=speconly specValidation=minimal`

## SEE ALSO

- [kdb(1)](kdb.md) for general information about the `kdb` tool.
- [elektra-spec(7)](elektra-spec.md) for an explanation of the `spec` namespace.
