# TESTING #

Libraries need a pervasive testing for continuous improvement. Any
problem found and behaviour described must be written down as test and
integrated within cmake so that on typing
 make test
one can be sure that all these promises hold.

The testing must happen on every level of the software to achieve a
maximum coverage with the available time. The 3 Levels of tests for
Elektra are:

1. Unit Tests which test a functional unit
2.  API Test which test the library
3.  Integration tests which test commandline tools built on the library

## Integration Tests ##



