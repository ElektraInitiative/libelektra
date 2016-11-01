# Testing #

## Introduction ##

Libraries need a pervasive testing for continuous improvement. Any
problem found and behaviour described must be written down as test and
integrated so that after running all tests in the build directory:

    make run_all

and on the target (installed) system:

    kdb run_all

it is assured that no new regressions were added.
To run memcheck tests run in the build directory:

    make run_memcheck

For tests in the build directory ctest is used, so you alternatively also can call
ctest with its options. On the target (installed) system our own scripts
drive the tests.

Some tests write into system paths.
If the access is denied, several tests will fail.
You have some options to avoid running them as root:

1. To void running the problematic test cases (reduces the test coverage!)
   run ctest without tests that have the label `kdbtests`:
   `ctest --output-on-failure -LE kdbtests`
2. To give your user the permissions to the relevant paths, i.e. (once as root):
   ```
   kdb mount-info
   chown -R `whoami` `kdb get system/info/constants/cmake/CMAKE_INSTALL_PREFIX`/`kdb get system/info/constants/cmake/KDB_DB_SPEC`
   chown -R `whoami` `kdb get system/info/constants/cmake/KDB_DB_SYSTEM`
   ```
   After that all test cases should run successfully as described above.
3. Compile Elektra so that system paths are not actual system paths, e.g. to write everything into
   the home directory (`~`) use cmake options:
   `-DKDB_DB_SYSTEM="~/.config/kdb/system" -DKDB_DB_SPEC="~/.config/kdb/spec"`
   (for another example with ini see `scripts/configure-home`)
4. Use the XDG resolver (see `scripts/configure-xdg`) and set
   the environment variable `XDG_CONFIG_DIRS`, currently lacks spec namespaces, see #734.

## Conventions ##

- All names of the test must start with test (needed by test driver for installed tests).
- No tests should run if ENABLE_TESTING is OFF.
- All tests that access harddisc:
 - should be tagged with kdbtests.
 - should not shall run, if `ENABLE_KDB_TESTING` is OFF.
 - should only write below `/tests` and `system/mountpoints`.
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
tests were introduced) in the `-full` variant. But when we run the
tests, we use `libelektra-full.so.0.8.9` (either by installing it or
by setting `LD_LIBRARY_PATH`). You can check with ldd which version is
used.

The tests are located [here](/tests/abi).

### C Unit Tests ###

C Unit Tests are written in plain C with the help of cframework.

It is used to test internal data structures of libelektra that are not
ABI relevant.

ABI tests can be done on theses tests, too. But by nature from time to
time these tests will fail.

They are located [here](/tests/ctest).

#### Internal Functions ####

According to `src/libs/elektra/libelektra-symbols.map`, all functions starting with:

* libelektra
* elektra
* kdb
* key
* ks

do not get exported. To test in internal functionality include the corresponding c file.

### Module Tests ###

The modules, which are typically used as plugins in Elektra (but can
also be available statically or in the `-full` variant), should have their
own tests.

Use the Cmake macro `add_plugintest` for adding these tests.

### C++ Unit Tests ###

C++ Unit tests are done using the gtest framework. See [architectural decision](/doc/decisions/unit_testing.md).

Use the CMake macro `add_gtest` for adding these tests.

### Script Tests ###

Script test are done using POSIX shell + CMake. See [architectural decision](/doc/decisions/script_testing.md).

The script tests have different purposes:
- End to End tests (usage of tools as a user would do)
- External Compilation tests (compile and run programs as a user would do)
- Conventions tests (do internal checks that check for common problems)
- Meta Test Suites (run other test suites)

### Other kind of Tests ###

Bindings, other than C++ typically have their own way of testing.

#### Fuzz Testing ####

Copy some import files to testcase_dir and run:

    /usr/src/afl/afl-1.46b/./afl-fuzz -i testcase_dir -o findings_dir bin/kdb import user/tests
