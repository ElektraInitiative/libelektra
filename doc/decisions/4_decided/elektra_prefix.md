# Elektra Prefix

## Problem

Some names, such as `kdbOpen`, `keyNew` are so generic that other libraries might also use them.
Furthermore, it is inconsistent that some functions have prefixes (e.g. `elektraKsFilter`) and others don't have (e.g. `ksNew`).

## Constraints

- In C such libraries, containing the same external identifier, cannot be used together.
- The [C99 standard, section 5.2.4.1](https://www.open-std.org/jtc1/sc22/wg14/) gives following limit:
  31 significant initial characters in an external identifier (each universal character name specifying a short identifier of 0000FFFF or less is considered 6 characters, each universal character name specifying a short identifier of 00010000 or more is considered 10 characters, and each extended source character is considered the same number of characters as the corresponding universal character name, if any)

## Assumptions

## Considered Alternatives

- leave it as is
- use macros to have short names for actually longer external identifiers
- use `e_` or `el_` as shorter prefix

## Decision

- Ensure all functions start with `elektra`.
- Ensure all macros and constants start with `ELEKTRA_`.

## Rationale

- This makes it clear which functions come from Elektra.
- Unifies all function names within Elektra (including core).
- Avoids collisions with other libs.

## Implications

- Changes in basically every application and tool, but this is automated with a refactoring tool @kodebach writes.
- To be 100% C99 compatible we cannot introduce identifiers which the same 31 character prefix (e.g. `ELEKTRA_WARNING_VALIDATION_SYNTACTIC_LEXER` and `ELEKTRA_WARNING_VALIDATION_SYNTACTIC_PARSER` would be a problem).
  Otherwise, it is not guaranteed that the code can be compiled with every C99 compiler.

## Related Decisions

## Notes
