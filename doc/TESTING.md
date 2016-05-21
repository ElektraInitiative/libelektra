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

The `testscr` and `testkdb` tests write in system pathes.
You have some options to avoid running them as root:

1. Avoid running these test cases, e.g. with, run ctest with:
   `ctest --output-on-failure -E '(testscr|testkdb)_.*`
2. Compile Elektra so that build-in pathes are not system pathes, use cmake options:
   `-DINSTALL_SYSTEM_FILES=OFF -DCMAKE_INSTALL_PREFIX=$WORKSPACE/local -DKDB_DB_SYSTEM=$WORKSPACE/system`
   (or via `scripts/configure-home`, currently pending #691)
3. Compile Elektra so that default pathes are not system pathes, use cmake options:
   `-DINSTALL_SYSTEM_FILES=OFF -DCMAKE_INSTALL_PREFIX=$WORKSPACE/local -DKDB_DB_SYSTEM=$WORKSPACE/system`
4. Use the XDG resolver (see `scripts/configure-xdg`) and set
   `XDG_CONFIG_DIRS`, currently lacks #734.
5. Give your user the permissions to the relevant pathes, i.e. (once as root):

   ```
   kdb mount-info
   chown -R `whoami` `kdb get system/info/constants/cmake/CMAKE_INSTALL_PREFIX`/`kdb get system/info/constants/cmake/KDB_DB_SPEC`
   chown -R `whoami` `kdb get system/info/constants/cmake/KDB_DB_SYSTEM`
   ```



## Conventions ##

- All names of the test must start with test
- No tests should run if ENABLE_TESTING is OFF.
- All tests that access harddisc:
 - should be tagged with kdbtests
 - should not shall run, if ENABLE_KDB_TESTING is OFF.
 - should only write below /tests ans system/mountpoints
- If your test has memleaks, e.g. because the library used leaks and
  that cannot be fixed, give them the label memleak with following
  command:

    set_property(TEST testname PROPERTY LABELS memleak)




## Strategy ##

The testing must happen on every level of the software to achieve a
maximum coverage with the available time. In the rest of the document
we describe the different levels and where these tests are.

### CFramework ###

This is basically a bunch of assertion macros and some output
facilities. It is written in pure C and very lightweight.

It is located [here](/tests/cframework).

### ABI Tests ###

C ABI Tests are written in plain C with the help of cframework.

The main purpose of these tests are, that the binaries of old versions
can be used against new versions as ABI tests.

So lets say we compile Elektra 0.8.8 (at this version dedicated ABI
tests were introduced) in the -full variant. But when we run the
tests, we use libelektra-full.so.0.8.9 (either by installing it or
by setting LD_LIBRARY_PATH). You can check with ldd which version is
used.

The tests are located [here](/tests/abi).


### C Unit Tests ###

C Unit Tests are written in plain C with the help of cframework.

It is used to test internal data structures of libelektra that are not
ABI relevant.

ABI tests can be done on theses tests, too. But by nature from time to
time these tests will fail.

They are located [here](/tests/ctest).


### Module Tests ###

The modules, which are typically used as plugins in Elektra (but can
also be available statically or in the -full variant), should have their
own tests.

Use the Cmake macro add_plugintest for adding these tests.


### C++ Unit Tests ###

C++ Unit tests are done using the gtest framework. See [architectural decision](/doc/decisions/unit_testing.md).

Use the CMake macro add_gtest for adding these tests.


### Script Tests ###

Script test are done using POSIX shell + CMake. See [architectural decision](/doc/decisions/script_testing.md).

The script tests have different purposes:
- End to End tests (usage of tools as a user would do)
- External Compilation tests (compile and run programs as a user would do)
- Conventions tests (do internal checks that check for common problems)
- Meta Test Suites (run other test suites)


### Other kind of Tests ###

Bindings, other than C++ typically have their own way of testing.

## Fuzz Testing

 copy some import files to testcase_dir and run:

 /usr/src/afl/afl-1.46b/./afl-fuzz -i testcase_dir -o findings_dir bin/kdb import user/tests
