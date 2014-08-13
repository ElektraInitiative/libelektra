# TESTING #

## Introduction ##

Libraries need a pervasive testing for continuous improvement. Any
problem found and behaviour described must be written down as test and
integrated so that after running all tests in the build directory:

    make run_all

and on the target (installed) system:

    kdb run_all

assures that all these promises hold.

To run memcheck tests use:

    make run_memcheck

In the build directory, internally ctest is used, so you can also call
ctest with its options. On the target (installed) system our own scripts
drive the tests.



## Conventions ##

- All names of the test must start with test
- No tests should run if ENABLE_TESTING is OFF.
- No tests that access harddisc shall run, if ENABLE_KDB_TESTING is OFF.
- If your test has memleaks, e.g. because the library used leaks and
  that cannot be fixed, give them the label memleak with following
  command:

    set_property(TEST testname PROPERTY LABELS memleak)



## Strategy ##

The testing must happen on every level of the software to achieve a
maximum coverage with the available time. In the rest of the document
we describe the different levels and where these tests are.

## C Unit Tests ###

C Unit Tests are written in plain C with the help of some assertion
macros.

## API Tests ###

## C++ Unit Tests ###

## Shell Tests ###

## Integration Tests ###

## Convention Tests ###


