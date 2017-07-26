# Testing

## Introduction

Libraries need pervasive testing for continuous improvement. Any
problem found and behavior described must be written down as test
so that it is assured that no new regressions will be added.


## Running Tests

Running all tests in the build directory:

    make run_all

And on the target (installed) system:

    kdb run_all

To run `memcheck` tests run in the build directory:

    make run_memcheck

They are supplementary, ideally you run all three.

Some tests write into system paths.
To avoid running tests that write to the disk you
can use:

    make run_nokdbtests

Directly running `ctest` might cause problems:
You need to set `LD_LIBRARY_PATH` as `run_all` and `run_nokdbtests` do.

If the access is denied, several tests will fail.
You have some options to avoid running them as root:

1. To avoid running the problematic test cases (reduces the test coverage!)
   run `ctest` without tests that have the label `kdbtests`:
   `ctest --output-on-failure -LE kdbtests`
   (which is also what `make run_nokdbtests` does)
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
   the environment variable `XDG_CONFIG_DIRS`, currently lacks `spec` namespaces, see #734.

## Conventions

- All names of the test must start with test (needed by test driver for installed tests).
- No tests should run if ENABLE_TESTING is OFF.
- All tests that access system/spec namespaces (e.g. mount something):
 - should be tagged with `kdbtests`:

        set_property(TEST testname PROPERTY LABELS kdbtests)

 - should not run, if `ENABLE_KDB_TESTING` is OFF.
 - should only write below
   - `/tests/<testname>` (e.g. `/tests/ruby`) and
   - `system/elektra` (e.g. for mounts or globalplugins).
 - clean up everything they change (in KDB and temporary files)
- If your test has memory leaks, e.g. because the library used leaks and
  they cannot be fixed, give them the label `memleak` with the following
  command:

        set_property(TEST testname PROPERTY LABELS memleak)


## Strategy

The testing must happen on every level of the software to achieve a
maximum coverage with the available time. In the rest of the document
we describe the different levels and where these tests are.

### CFramework

This is basically a bunch of assertion macros and some output
facilities. It is written in pure C and very lightweight.

It is located [here](/tests/cframework).

### ABI Tests

C ABI Tests are written in plain C with the help of `cframework`.

The main purpose of these tests are, that the binaries of old versions
can be used against new versions as ABI tests.

So lets say we compile Elektra 0.8.8 (at this version dedicated ABI
tests were introduced) in the `-full` variant. But when we run the
tests, we use `libelektra-full.so.0.8.9` (either by installing it or
by setting `LD_LIBRARY_PATH`). You can check with `ldd` which version is
used.

The tests are located [here](/tests/abi).

### C Unit Tests

C Unit Tests are written in plain C with the help of `cframework`.

It is used to test internal data structures of libelektra that are not
ABI relevant.

ABI tests can be done on theses tests, too. But by nature from time to
time these tests will fail.

They are located [here](/tests/ctest).

#### Internal Functions

According to `src/libs/elektra/libelektra-symbols.map`, all functions starting with:

* libelektra
* elektra
* kdb
* key
* ks

get exported. Functions not starting with this prefix are internal only and therefore
not visible in the test cases. Test internal functionality by including the corresponding C file.

### Module Tests

The modules, which are typically used as plugins in Elektra (but can
also be available statically or in the `-full` variant), should have their
own tests.

Use the CMake macro `add_plugintest` for adding these tests.

### C++ Unit Tests

C++ Unit tests are done using the Google Test framework.
See [architectural decision](/doc/decisions/unit_testing.md).

Use the CMake macro `add_gtest` for adding these tests.

### Script Tests

Tests which need scripts are done using shell recorder or directly with POSIX shell commands.
See [architectural decision](/doc/decisions/script_testing.md).

The script tests have different purposes:

- End to End tests (usage of tools as an end user would do)
- External compilation tests (compile and run programs as a user would do)
- Conventions tests (do internal checks that check for common problems)
- Meta Test Suites (run other test suites)

See [here](/tests/shell).

### Shell Recorder

The more elegant way to specify script tests are via the so called shell recorder
using Markdown Syntax.

See [here](/tests/shell/shell_recorder/tutorial_wrapper/README.md).

### Fuzz Testing

Copy some import files to `testcase_dir` and run:

    /usr/src/afl/afl-1.46b/./afl-fuzz -i testcase_dir -o findings_dir bin/kdb import user/tests

### ASAN

To enable sanitize checks use `ENABLE_ASAN` via cmake.

Then, to use ASAN, run `run_asan` in the build directory, which simply does:

	ASAN_OPTIONS=symbolize=1 ASAN_SYMBOLIZER_PATH=$(shell which llvm-symbolizer) make run_all

It could also happen that you need to preload ASAN library, e.g.:

	LD_PRELOAD=/usr/lib/clang/3.8.0/lib/linux/libclang_rt.asan-x86_64.so run_asan

or on Debian:

	LD_PRELOAD=/usr/lib/llvm-3.8/lib/clang/3.8.1/lib/linux/libclang_rt.asan-x86_64.so run_asan

See also build server jobs:

* [clang-asan](https://build.libelektra.org/job/elektra-clang-asan/)
* [gcc-asan](https://build.libelektra.org/job/elektra-gcc-asan/)

### CBMC

For bounded model checking tests, see `scripts/cbmc`.

### Static Code Checkers

There is a number of static code checkers available for all kind of programming languages. The
following section show how the most common ones can be used with `libelektra`.

#### Cppcheck

[Cppcheck](http://cppcheck.sourceforge.net/) can be used directly on a C or C++ source
file by calling it with `cppcheck --enable=all <sourcefile>`. This way it might miss some header
files though and thus doesn't detect all possible issues, but still gives useful hints in general.

To analyze the whole project, use it in conjunction with `cmake` by calling `cmake` with the parameter
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`. This way `cmake` creates a file called `compile_commands.json` in
the build directory. Afterwards, call `cppcheck` with the cmake settings and store the output as xml:

    cppcheck --project=compile_commands.json --enable=all -j 8 --xml-version=2 2> cppcheck_result.xml

Since the XML file is difficult to read directly, the best way is to convert it to an HTML report.
Cppcheck already includes a tool for that, call it with the XML report:

    cppcheck-htmlreport --file=cppcheck_result.xml --report-dir=cppcheck_report --source-dir=.

Now you can view the html report by opening `index.html` in the specified folder to get an overview
of the issues found in the whole project.

#### OCLint

[OCLint](http://oclint.org/) is a static code analyzer for C, C++ and Objective C. To use this tool enable the CMake option `CMAKE_EXPORT_COMPILE_COMMANDS`. The steps below show a step-by-step guide on how to analyze files with OCLint.

1. Create a build directory if you have not done so already and change the working path to this directory:

   ```sh
   mkdir -p build
   cd build
   ```

2. Run CMake with the option `CMAKE_EXPORT_COMPILE_COMMANDS`:

   ```sh
   cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
   ```

3. Build Elektra

   ```sh
   make
   ```

4. Run the `oclint` command specifying the files you want to analyze

   ```sh
   cd ..
   oclint -p build -no-analytics -enable-global-analysis -enable-clang-static-analyzer src/plugins/ini/*.c
   ```

#### scan-build

[scan-build](http://clang-analyzer.llvm.org/scan-build.html) is a tool that is usually bundled along
with LLVM/Clang and is also primarily intended for C and C++ code. On macOS you have to install the
package `llvm` with homebrew, then you'll find the tool in the folder `/usr/local/opt/llvm/bin/`.

To use it, change the C compiler and the C++ compiler to the LLVM analyzer. To do this, you can
configure the project from scratch and prefix the cmake command with `scan-build`. Alternatively, set
the c compiler to `ccc-analyzer` and the C++ compiler to `c++-analyzer` (bundled with LLVM/Clang).

Then you can build the project with `make` like usual, prefixing the command with `scan-build`.
The `-o` option specifies where the html results get stored. Ensure you build the project from scratch,
otherwise the analyzation might be incomplete.

    scan-build -o ./scanbuild_result make -j 4

Afterwards, the report can be viewed by using the tool `scan-view`, also found in the llvm folder.
The report is created in the folder specified above, along with the current date of the analyzation,
for instance:

    scan-view <path specified above>/2017-06-18-171027-27108-1

Alternatively, you can also open the `index.html` file in the aforementioned folder, but using the tool
the report is enriched with further information.

#### SonarLint

[SonarLint](http://www.sonarlint.org/) is a static code checker primarily intended for Java. It is
usually used by installing the corresponding plugin for the used IDE, then there is no further
configuration required.

### Code Coverage

Run:

	make coverage-start
	# now run all tests! E.g.:
	make run_all
	make coverage-stop
	make coverage-genhtml

The HTML files can be found in the build directory in the folder `coverage`.

See also the build server job:

* [gcc-asan](https://build.libelektra.org/job/elektra-incremental/)


## See also

- [COMPILE](COMPILE.md).
- [INSTALL](INSTALL.md).
