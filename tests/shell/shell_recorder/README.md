# Shell Recorder

## Overview

The Shell Recorder is a test tool that checks the output and return values of shell commands such as `kdb`. It allows you to write unit and
regression tests. Lets take a look at a simple Shell Recorder test first. We store the text:

```
Mountpoint: user/examples/shellrecorder

STDOUT: Create a new key user/examples/shellrecorder/key with string "value"
RET: 0
< kdb set user/examples/shellrecorder/key value
```

in a file called `test.esr` in the folder `Documents` in the home directory (`~/Documents/test.esr`). Shell Recorder tests start with a
list of global values. The only required value is `Mountpoint`. It specifies the location in the KDB that the Shell Recorder will save
before it runs the tests and restore after it is finished. In our example the Shell Recorder will backup and restore everything below the
namespace `user/examples/shellrecorder`. After the global values a Shell Recorder file contains a list of tests.

As you can see above, we specify the command we want to test after an initial smaller-than sign (`<`). In our case we want to test the
command `kdb set /examples/shellrecorder/key value`. The words above the command are directives that tell the Shell Recorder what it should
test. In our case we want to make sure, that the command prints the text
`Create a new key user/examples/shellrecorder/key with string"value"` to `stdout`, and that the exit code of the command (return value)
is `0`.

Before we use the Shell Recorder we need to [build Elektra](/doc/COMPILE.md). If we assume that we built Elektra in the root of the
repository in a folder called `build`, then the Shell Recorder is located at `build/tests/shell/shell_recorder/shell_recorder.sh`. To start
our test we call the Shell Recorder from the root of the repository. The root directory of the repo is also the **default working directory** for tests (`*.esr`) located in this folder. We specify our test file as argument for the Shell Recorder:

```sh
build/tests/shell/shell_recorder/shell_recorder.sh ~/Documents/test.esr
```

. The Shell Recorder should then print something like the following text:

```
kdb set user/examples/shellrecorder/key value
shell_recorder /Users/rene/Documents/test.esr RESULTS: 2 test(s) done 0 error(s).
```

. If we want to check that the Shell Recorder really works we can modify the test:

```
Mountpoint: user/examples/shellrecorder

STDOUT: NaNaNaNaNaNaNa
RET: 1337
< kdb set user/examples/shellrecorder/key value
```

. Now the output should look something like this:

```
kdb set user/examples/shellrecorder/key value
Return value “0” does not match “1337”

ERROR - STDOUT:
“Create a new key user/examples/shellrecorder/key with string "value"”
does not match
“NaNaNaNaNaNaNa”

shell_recorder /Users/rene/Documents/test.esr RESULTS: 2 test(s) done 2 error(s).
📕  Protocol File: /var/folders/hx/flbncdhj4fs87095gzxvnj3h0000gn/T/elektraenv.XXXXXXXXX.gWyTCr2O
```

. We see that both checks failed. The protocol file at the end of the output contain the real output and  return value of the command:

```
…
RET: 0
…
STDOUT: Create a new key user/examples/shellrecorder/key with string "value"
…
```

.

## Configuration

You can use the global values below at the start of Shell Recorder test. The basic syntax is `Variable: Value`.

### Mountpoint

This is the only configuration variable that has to be set. It is used to determine where the `shell_recorder` should look for changes.
e.g. `Mountpoint: user/test` tells `shell_recorder` that you will be working under `user/test`.

### DiffType

Use either `Dump` or `Ini` to tell the Shell Recorder to

1. export the keys below `Mountpoint` using the Dump or INI format,
2. and diff the changes with the last recorded output.

If you use the option `File`, then Shell Recorder does a diff on the configuration file mounted to `Mountpoint`.

### File

Tells `shell_recorder` what file it should use for diffs.
If `File` is present but empty a fresh database file will be provided for every run.

## Checks

Posix-extended regular expressions are used to check and validate return values and outputs.

**Remark:** Shell Recorder uses the `⏎` symbol as line terminator. This means that you need to use the character `⏎` (instead of `\n`) if
you want to match a line ending in a multiline output. For example: Assume there are exactly two keys with the name `key1` and `key2`
located under the path `user/test`. The output of the command `kdb ls user/test` would then be the following

```
user/test/key1
user/test/key2
```

. You can check this exact output in a Shell Recorder script via the following code:

```
STDOUT: user/test/key1⏎user/test/key2
< kdb ls user/test
```

. If you only want to check that `key1` and `key2` are part of the output you can use the regex `key1.*key2` instead:

```
STDOUT-REGEX: key1.*key2
< kdb ls user/test
```

. As you can see the line ending is considered  a normal character (`.`) in the output.

### Options

The Shell Recorder provides the following checks

- `STDOUT:` The Shell Recorder matches the **text** after this directive 1:1 with the standard output of the command.
- `STDOUT-REGEX:` Use this directive if you want to compare the standard output of the command with a **regular expression**.
* `STDERR:` The Shell Recorder compares the **regular expression** after this directive with the standard error output of the command.
* `RET:` The Shell Recorder compares this **regular expression** with the return code (exit status) of the command.
* `WARNINGS:` This **comma separated list** of numbers is compared with the warnings thrown by a `kdb` command.
* `ERROR:` The Shell Recorder compares this number to the error thrown by a `kdb` command.
* `DIFF:` This **regular expression** is compared with the output of a `diff` command. The input for the `diff` command is the last and current state of the key database. The directive `DiffType` specifies if the the Shell Recorder compares the file of the storage plugin (`File`) directly, or if it uses the INI (`Ini`) or Dump (`Dump`) format.

## Commands

A command starts with `<` followed by kdb or shell commands.

## Examples

Please take a look at the examples files (`*.esr`) located inside this folder.
