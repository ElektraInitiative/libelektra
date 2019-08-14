# Binding Implementation

## Problem

In the previous error concept it was very useful to generate macros as we often added new errors. The code generation itself, however, is quite complicated (C++ code that prints the code; which is also not ideal for cross compilation, see also https://github.com/ElektraInitiative/libelektra/issues/2814).

## Constraints

## Assumptions

- Error codes do not change very often

## Considered Alternatives

- Migrate to a more modern and easier way to generate code with our mustache system, and let this generate the (mapping) code for all compiled languages (C, C++, Java, Rust, Go).
  All we get out of this is the removal of `std::cout << ...` code from C++ but not much more.

- Migrate to CMake code that generates such macros/classes (see also https://github.com/ElektraInitiative/libelektra/issues/2814)
  Approach will have problems with languages that have more refined error concepts.

## Decision

Write down the few macros manually, and also manually write down exceptions for the language bindings (and also the mappings from Elektra's internal errors to nice errors specific for the languages)

Since error codes and crucial parts Elektra's core implementation will not change often this is the best approach with minimal effort.

## Rationale

- Updates to error codes will imply syncing every binding. This though should happen in very rare circumstances.

## Implications

- The current bindings stay untouched
- Tutorial on how to write a binding should be written

## Related Decisions

## Notes

- https://github.com/ElektraInitiative/libelektra/issues/2871
- https://github.com/ElektraInitiative/libelektra/issues/2814
