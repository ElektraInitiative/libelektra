# Testing

## Introduction

Libraries need pervasive testing for continuous improvement. Any
problem found and behavior described must be written down as test
so that it is assured that no new regressions will be added.

## Running Tests

Running all tests in the build directory:

```sh
make run_all
```

And on the target (installed) system:

```sh
kdb run_all
```

To run `memcheck` tests run in the build directory:

```sh
make run_memcheck
```

They are supplementary, ideally you run all three.

Some tests write into system paths and into the home directory.
This implies that the UID running the tests must have a home directory.
To avoid running tests that write to the disk you
can use:

```sh
make run_nokdbtests
```

You can also directly run ctest to make use of parallel testing:

```sh
ctest -T Test --output-on-failure -j 6
ctest -T MemCheck -LE memleak --output-on-failure -j 6
```

The alternative to `make run_nokdbtests`:

```sh
ctest -T Test --output-on-failure -LE kdbtests -j 6
```

To only run tests whose names match a regular expression, you can use:

```sh
ctest -V -R <regex>
```

## Required Environment

To run the tests successfully, the environment
must fulfill:

- Mounted /dev and /proc (to have stdin and stdout for import & export test cases).
- POSIX tools need to be available (including the `file` tool)
- User must be able to write to system and spec (see below)

If the access is denied, several tests will fail.
You have some options to avoid running them as root:

1. To avoid running the problematic test cases (reduces the test coverage!)
   run `ctest` without tests that have the label `kdbtests`:
   `ctest --output-on-failure -LE kdbtests`
   (which is also what `make run_nokdbtests` does)
2. To give your user the permissions to the relevant paths execute the lines
   below once as root.

   **Warning: Changing permissions on the wrong paths can be harmful! Please make
   sure that the paths are correct.**
   In doubt make sure that you have a backup of the affected directories.

   First load the required information and verify the paths:

   ```sh
   sudo kdb mount-info
   echo `kdb sget system:/info/elektra/constants/cmake/CMAKE_INSTALL_PREFIX .`/`kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .`
   echo `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .`
   ```

   Then change the permissions:

   ```sh
   sudo chown -R `whoami` `kdb sget system:/info/elektra/constants/cmake/CMAKE_INSTALL_PREFIX .`/`kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .`
   sudo chown -R `whoami` `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .`
   ```

   After that all test cases should run successfully as described above.

3. Compile Elektra so that system paths are not actual system paths, e.g. to write everything into
   the home directory (`~`) use cmake options:
   `-DKDB_DB_SYSTEM="~/.config/kdb/system" -DKDB_DB_SPEC="~/.config/kdb/spec"`
   (for an example of a full CMake invocation see `scripts/configure-home`)
4. Use the XDG resolver (see `scripts/configure-xdg`) and set
   the environment variable `XDG_CONFIG_DIRS`, currently lacks `spec` namespaces, see #734.

## Manual Testing

Running executables in the build directory needs some preparation.
Here we assume that `build` is the build directory and it is the
top-level of Elektra's source code:

```
cd build
. ../scripts/dev/run_env
```

After sourcing `run_env`, you can directly execute `kdb` and other
binaries built with Elektra (such as the examples).

Pay attention that sourcing depends on the operating system or rather the
shell. For example on standard FreeBSD 11.3 you have to execute `sh` in the
root of the repository first. Then do _not_ use the `source` command but the
point `.` as explained above.

## Using debuggers

You can use debuggers such as `gdb` to develop Elektra.
This can be interesting if you write test cases and they fail at some unknown point.
Many tests are put into the `/bin` folder in your build directory.
As a consequence, you can `cd` into `/bin` and call, for example

```
gdb hello
```

If you use [Docker to run your tests](/doc/tutorials/run_all_tests_with_docker.md) it makes sense to call the debugger in the same container.
In this case you might be required to pass additional parameters to Docker.
An example for this is passing `--security-opt seccomp=unconfined` to disable address space randomization.

## Recommended Environment

The tests are designed to disable themselves if some necessary tools are
missing or other environmental constraints are not met. To really run
_all_ tests (also those that are mostly designed for internal development)
you need to fulfil:

- Elektra must be installed (for gen + external test cases).
- A running dbus daemon (Either "system" or "session" daemon).
- `gpg2` or `gpg` binary must be available.

Above environment is needed for both `kdb run_all` (installed test cases)
and `make run_all` (test cases executed from the build directory).
For `make run_all` following development tools enable even more tests:

- The script `checkbashisms` is needed to check for bashism (`tests/shell/check_bashisms.sh`),
  it is part of [`devscripts`](https://packages.debian.org/devscripts).
- The [POSIX compatibility test for shell scripts](../tests/shell/check_posix.sh) requires the tool [`shfmt`](https://github.com/mvdan/sh).
- `git`, `clang-format` (version 12), and [cmake-format](https://github.com/cheshirekow/cmake_format) to check formatting.
- `pkg-config` must be available (`check_external.sh` and `check_gen.sh`).
- A build environment including gcc (`check_gen.sh`).
- The [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper)
  requires POSIX utilities (`awk`, `grep`, …).

You can also use Docker to set up such an environment.
There is [a tutorial](/doc/tutorials/run_all_tests_with_docker.md) that guides you through the necessary steps.

## Adding Tests

For plugins, adding `ADD_TEST` to `add_plugin` will execute the tests in `testmod_${pluginname}.c`.
This is done by default for newly generated plugins.

Add `CPP_TEST` if the test is written in C++.
Then `testmod_${pluginname}.cpp` will be used.
These tests use the [gtest](https://github.com/google/googletest) test framework.

If the tests should not always be executed, the CMake function
`add_plugintest` can be used instead.
See `scripts/cmake/Modules/LibAddPlugin.cmake` for more information.

By using `TEST_README` in `add_plugin` (also enabled by default),
[Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper)
are expected to be in the README.md of the plugin.

## Conventions

- All names of the test must start with test (needed by test driver for installed tests).
- No tests should run if ENABLE_TESTING is OFF.
- All tests that write to `system:` or `spec:` namespaces (e.g. with `kdbSet` or by mounting):
  should be tagged with `kdbtests`:

  ```cmake
  set_property(TEST testname PROPERTY LABELS kdbtests)
  ```

- should not run, if `ENABLE_KDB_TESTING` is OFF.
- should only write below
  - `/tests/<testname>` (e.g. `/tests/ruby`) and
  - `system:/elektra` (e.g. for mounts or globalplugins).
- Before executing tests, no keys must be present below `/tests`.
  The test cases need to clean up everything they wrote.
  (Including temporary files)
- If your test has memory leaks, e.g. because the library used leaks and
  they cannot be fixed, give them the label `memleak` with the following
  command:

  ```cmake
  set_property(TEST testname PROPERTY LABELS memleak)
  ```

- If your test modifies resources needed by other tests you also need to set
  `RUN_SERIAL`:

  ```cmake
  set_property(TEST testname PROPERTY RUN_SERIAL TRUE)
  ```

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

ABI tests can be done on these tests, too. But by nature from time to
time these tests will fail.

They are located [here](/tests/ctest).

#### Internal Functions

According to `src/libs/elektra/libelektra-symbols.map`, all functions starting with:

- `libelektra`
- `elektra`
- `kdb`
- `key`
- `ks`

get exported. Functions not starting with this prefix are internal only and therefore
not visible in the test cases. Test internal functionality by including the corresponding C file.

### Module Tests

The modules, which are typically used as plugins in Elektra (but can
also be available statically or in the `-full` variant), should have their
own tests.

Use the CMake macro `add_plugintest` for adding these tests.

### C++ Unit Tests

C++ Unit tests are done using the Google Test framework.
See [architectural decision](/doc/decisions/5_implemented/unit_testing.md).

Use the CMake macro `add_gtest` for adding these tests.

### Script Tests

Tests which need scripts are done using shell recorder or directly with POSIX shell commands.
See [architectural decision](/doc/decisions/5_implemented/script_testing.md).

The script tests have different purposes:

- End to End tests (usage of tools as an end user would do)
- External compilation tests (compile and run programs as a user would do)
- Conventions tests (do internal checks that check for common problems)
- Meta Test Suites (run other test suites)

See [here](/tests/shell).

### Shell Recorder

The more elegant way to specify script tests are via the so-called Shell Recorder
using Markdown Syntax.

See [here](/tests/shell/shell_recorder/tutorial_wrapper/README.md).

### Fuzz Testing

We assume that your current working directory is a newly created
build directory. First compile Elektra with afl
(~e is source-dir of Elektra):

```sh
~e/scripts/dev/configure-debian -DCMAKE_C_COMPILER=/usr/src/afl/AFL-2.57b/afl-gcc -DCMAKE_CXX_COMPILER=/usr/src/afl/AFL-2.57b/afl-g++ ~e
make -j 5
```

Copy some import files to `testcase_dir`, for example:

```sh
mkdir -p testcase_dir
cp ~e/src/plugins/toml/toml/* testcase_dir
```

Fewer files is better. Then run, for example:

```sh
LD_LIBRARY_PATH=`pwd`/lib /usr/src/afl/AFL-2.57b/afl-fuzz -i testcase_dir -o findings_dir bin/kdb import user:/tests toml
```

Check if something is happening with:

```sh
watch kdb export user:/tests
```

### ASAN

To enable sanitize checks use `ENABLE_ASAN` via cmake.

Then, to use ASAN, run `run_asan` in the build directory, which simply does:

```sh
ASAN_OPTIONS=symbolize=1 ASAN_SYMBOLIZER_PATH=$(which llvm-symbolizer) make run_all
```

It could also happen that you need to preload ASAN library, e.g.:

```sh
LD_PRELOAD=/usr/lib/clang/3.8.0/lib/linux/libclang_rt.asan-x86_64.so run_asan
```

or on Debian:

```sh
LD_PRELOAD=/usr/lib/llvm-3.8/lib/clang/3.8.1/lib/linux/libclang_rt.asan-x86_64.so run_asan
```

#### macOS

If you use macOS you might want to use the `clang` versions provided by Homebrew, since it supports the [LeakSanitizer](https://github.com/google/sanitizers/wiki/AddressSanitizerLeakSanitizer). To use Homebrew’s version of `clang` you need to first install LLVM:

```sh
brew install llvm
```

. After that change the `CC` and `CXX` environment variables to point to the clang tools provided by LLVM:

```sh
export CC=/usr/local/opt/llvm/bin/clang
export CXX=/usr/local/opt/llvm/bin/clang++
```

. Now run CMake and build Elektra just like you normally would. To enable the Leak Sanitizer you need to also set the variable `ASAN_OPTIONS` before you run a test:

```sh
export ASAN_OPTIONS=detect_leaks=1
```

.

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

```sh
cppcheck --project=compile_commands.json --enable=all -j 8 --xml-version=2 2> cppcheck_result.xml
```

Since the XML file is difficult to read directly, the best way is to convert it to an HTML report.
Cppcheck already includes a tool for that, call it with the XML report:

```sh
cppcheck-htmlreport --file=cppcheck_result.xml --report-dir=cppcheck_report --source-dir=.
```

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
   oclint -p build -no-analytics -enable-global-analysis -enable-clang-static-analyzer src/plugins/toml/*.c
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

```sh
scan-build -o ./scanbuild_result make -j 4
```

Afterwards, the report can be viewed by using the tool `scan-view`, also found in the llvm folder.
The report is created in the folder specified above, along with the current date of the analyzation,
for instance:

```sh
scan-view <path specified above>/2017-06-18-171027-27108-1
```

Alternatively, you can also open the `index.html` file in the aforementioned folder, but using the tool
the report is enriched with further information.

#### SonarLint

[SonarLint](http://www.sonarlint.org/) is a static code checker primarily intended for Java. It is
usually used by installing the corresponding plugin for the used IDE, then there is no further
configuration required.

### Randoop

For using the unit test generator randoop with the jna bindings, see `scripts/randoop/randoop`.

### Code Coverage

Run:

```sh
make coverage-start
# now run all tests! E.g.:
make run_all
make coverage-stop
make coverage-genhtml
```

The HTML files can be found in the build directory in the folder `coverage`.

## See Also

- [COMPILE](COMPILE.md).
- [INSTALL](INSTALL.md).
- [BUILDSERVER](BUILDSERVER.md).
