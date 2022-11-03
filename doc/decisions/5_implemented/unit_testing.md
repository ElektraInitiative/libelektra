# C++ Unit Testing Framework

## Problem

The previous unit testing framework started as hack to have a bit more
than simple asserts. It is not easy to use (needs explicit enumeration
of all test cases) and lacks really important features (e.g. output of
the assertion that failed).

## Constraints

- Must be BSD licenced
- Must be easy to use
- should be portable
- container testing?
- mocking?

## Assumptions

## Considered Alternatives

- Continue with current framework
- Boost Unit Testing framework

## Decision

- Keep C framework for C tests and ABI tests
- Google Unit testing framework `gtest` with code downloaded by CMake for
  systems where no source is packaged (Debian Wheezy, Arch Linux,
  Fedora,...) for C++ tests

## Rationale

- Having the output of current values when an assertion fails in any case
- No listing of all test cases in main (but instead having test discovery)
- No more commenting out if you only want to run parts of the test-suite
- No more typos in test-suite namings
- xUnit output for jenkins
- value and type-parameterized tests
- Mock-Support (not available in gtest?)
- setup/teardown global+per test
- supports death tests
- writing many parts of it on our own adds to the total amount of code to write and maintain.
- integrations into IDEs

## Implications

- It adds lots of code in the repository
- It is not ideal to have different frameworks intermixed (C vs. C++ frameworks, but most code is C)
- In the end we have to write a lot of functionality ourselves anyway (e.g. comparing Keys and KeySets)
- Testsuite execution are already handled by cmake and kdb run-all.
- The selection of tests within a test suite does not play well with ctest.
- Rewriting all current tests to have unified behavior is a lot of work
- Won't work for ABI compatibility tests
- Mock only by extra framework

## Related Decisions

- [Script Testing](script_testing.md)

## Notes

- We had discussions on Mailinglists
- We had discussions on [GitHub](https://github.com/ElektraInitiative/libelektra/pull/26)
