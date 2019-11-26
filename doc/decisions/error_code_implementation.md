# Error Code Implementation

## Problem

In the previous error concept it was very useful to generate macros as we often added new errors. The code generation itself, however, is quite complicated (C++ code that prints the code; which is also not ideal for cross compilation, see also https://github.com/ElektraInitiative/libelektra/issues/2814).

## Constraints

## Assumptions

- Error codes do not change very often

## Considered Alternatives

- Migrate to a more modern and easier way to generate code with our mustache system, and let this generate the (mapping) code for all compiled languages (C, C++, Java, Rust, Go).
  All we get out of this is the removal of `std::cout << ...` code from C++ but not much more.

- Migrate to CMake code that generates such macros/classes (see also https://github.com/ElektraInitiative/libelektra/issues/2814)
  Mustache templates have to be supplied with the input data somehow. Either we have to use a custom executable that is compiled at build time.
  In that case we would just get rid of the std::cout << ... in the C++ code, but not much else would change.
  The other option is to use the default mustache executable, which is a Ruby script and therefore requires Ruby to be installed.
  Also kdb gen cannot be reused, since that would require compiling kdb first, which needs kdberrors.h.

## Decision

Write down the few macros manually, and also manually write down exceptions for the language bindings (and also the mappings from Elektra's internal errors to nice errors specific for the languages)

Since error codes and crucial parts Elektra's core implementation will not change often this is the best approach with minimal effort.

The existing code will be refactored so that error macros directly call macros.

When adding a new error code, language bindings have to be adapted accordingly such as the Rust or Java Binding.

## Rationale

- Updates to error codes will imply syncing every binding. This though should happen in very rare circumstances.

## Implications

- The current bindings stay untouched
- Tutorial on how to write a binding should be written

## Related Decisions

## Notes

- https://github.com/ElektraInitiative/libelektra/issues/2871
- https://github.com/ElektraInitiative/libelektra/issues/2814
