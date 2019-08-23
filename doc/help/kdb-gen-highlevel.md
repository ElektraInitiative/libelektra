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

[//]: # "TODO [kodebach]"

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

[//]: # "TODO"

## SEE ALSO

- [kdb(1)](kdb.md) for general information about the `kdb` tool.
- [elektra-spec(7)](elektra-spec.md) for an explanation of the `spec` namespace.
