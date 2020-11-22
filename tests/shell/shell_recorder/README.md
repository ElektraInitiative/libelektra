# Shell Recorder

## Overview

The Shell Recorder is a test tool that checks the output and return values of shell commands such as `kdb`. It allows you to write unit and
regression tests.

Generally you do not want to directly use the shell recorder but use the [Markdown format](tutorial_wrapper/README.md) instead.

## Example

Lets take a look at a simple Shell Recorder test first. We store the text:

```
Mountpoint: user:/examples/shellrecorder

STDOUT: Create a new key user:/examples/shellrecorder/key with string "value"
RET: 0
< kdb set user:/examples/shellrecorder/key value
```

in a file called `test.esr` in the folder `Documents` in the home directory (`~/Documents/test.esr`). Shell Recorder tests start with a
list of global values. The only required value is `Mountpoint`. It specifies the location in the KDB that the Shell Recorder will save
before it runs the tests and restore after it is finished. In our example the Shell Recorder will backup and restore everything below the
namespace `user:/examples/shellrecorder`. After the global values a Shell Recorder file contains a list of tests.

As you can see above, we specify the command we want to test after an initial smaller-than sign (`<`). In our case we want to test the
command `kdb set /examples/shellrecorder/key value`. The words above the command are directives that tell the Shell Recorder what it should
test. In our case we want to make sure, that the command prints the text
`Create a new key user:/examples/shellrecorder/key with string"value"` to `stdout`, and that the exit code of the command (return value)
is `0`.

Before we use the Shell Recorder we need to [build Elektra](/doc/COMPILE.md). If we assume that we built Elektra in the root of the
repository in a folder called `build`, then the Shell Recorder is located at `build/tests/shell/shell_recorder/shell_recorder.sh`. To start
our test we call the Shell Recorder from the root of the repository. The root directory of the repo is also the **default working directory** for tests (`*.esr`) located in this folder. We specify our test file as argument for the Shell Recorder:

```sh
build/tests/shell/shell_recorder/shell_recorder.sh ~/Documents/test.esr
```

. The Shell Recorder should then print something like the following text:

```
kdb set user:/examples/shellrecorder/key value
shell_recorder /Users/rene/Documents/test.esr RESULTS: 2 test(s) done 0 error(s).
```

. If we want to check that the Shell Recorder really works we can modify the test:

```
Mountpoint: user:/examples/shellrecorder

STDOUT: NaNaNaNaNaNaNa
RET: 1337
< kdb set user:/examples/shellrecorder/key value
```

. Now the output should look something like this:

```
kdb set user:/examples/shellrecorder/key value

ERROR - RET:
Return value “0” does not match “1337”


ERROR - STDOUT:
“Create a new key user:/examples/shellrecorder/key with string "value"”
does not match
“NaNaNaNaNaNaNa”

shell_recorder /Users/rene/Documents/test.esr RESULTS: 2 test(s) done 2 error(s).

—— Protocol ————————————————————————————————————————————————————
CMD: kdb set user:/examples/shellrecorder/key value
RET: 0
=== FAILED return value does not match expected pattern 1337
STDOUT: Create a new key user:/examples/shellrecorder/key with string "value"
=== FAILED stdout does not match expected pattern NaNaNaNaNaNaNa
————————————————————————————————————————————————————————————————
```

. We see that both checks failed. The protocol at the end of the output contain the real output and return value of the command.

## Configuration

You can use the global values below at the start of Shell Recorder test. The basic syntax is `Variable: Value`.

### Mount Point

This is the only configuration variable that has to be set. It is used to determine where the `shell_recorder` should look for changes.
e.g. `Mountpoint: user:/test` tells `shell_recorder` that you will be working under `user:/test`.

## Checks

Posix-extended regular expressions are used to check and validate return values and outputs.

**Remark:** Shell Recorder uses the `⏎` symbol as line terminator. This means that you need to use the character `⏎` (instead of `\n`) if
you want to match a line ending in a multiline output. For example: Assume there are exactly two keys with the name `key1` and `key2`
located under the path `user:/test`. The output of the command `kdb ls user:/test` would then be the following

```
user:/test/key1
user:/test/key2
```

. You can check this exact output in a Shell Recorder script via the following code:

```
STDOUT: user:/test/key1⏎user:/test/key2
< kdb ls user:/test
```

. If you only want to check that `key1` and `key2` are part of the output you can use the regex `key1.*key2` instead:

```
STDOUT-REGEX: key1.*key2
< kdb ls user:/test
```

. As you can see the line ending is considered a normal character (`.`) in the output.

### Options

The Shell Recorder provides the following checks

- `STDOUT:` The Shell Recorder matches the **text** after this directive 1:1 with the standard output of the command.
- `STDOUT-REGEX:` Use this directive if you want to compare the standard output of the command with a **regular expression**.

* `STDERR:` The Shell Recorder compares the **regular expression** after this directive with the standard error output of the command.
* `RET:` The Shell Recorder compares this **regular expression** with the return code (exit status) of the command.
* `WARNINGS:` This **comma separated list** of numbers is compared with the warnings thrown by a `kdb` command.
* `ERROR:` The Shell Recorder compares this number to the error thrown by a `kdb` command.

## Commands

A command starts with `<` followed by `kdb` or shell commands.

### Multiline Commands

The Shell Recorder supports multiline commands. Just add an additional line, a `<` characters and continue with your command. For
example, the following text specifies a multiline command spanning over three lines:

```
> cat <<EOF
> The Raging Sun
> EOF
```

. To separate commands either add a check (such as `RET:`) or at least one empty line. For example, the following text specifies three
commands, the last of them being a multiline command:

```
> echo 'I Knew'
STDOUT: You
> echo 'You'

> echo 'Were'
> echo 'Trouble'
```

.

## Examples

Please take a look at the examples files (`*.esr`) located inside this folder.

## Replay Tests

If you want to create a test for `kdb` commands, but you do not want to write down the standard output, return value and the other things
the Shell Recorder compares, then you can use protocol files to create a **replay test**. Start by writing down the commands you want to
test. In the following example we want to verify the behavior of the command `kdb ls`. We create a file called `ls.esr` containing a
mountpoint and a list of commands:

```
Mountpoint: user:/test/ls

< kdb set user:/test/ls/level1 'one'

< kdb ls user:/test/ls

< kdb set user:/test/ls/level1/level2 'two'

< kdb set user:/test/ls/the 'roots'

< kdb ls user:/test/ls

< kdb set user:/test/ls/the/next/level
```

. We then execute the test with the Shell Recorder using the command line switch (`-p`):

```sh
build/tests/shell/shell_recorder/shell_recorder.sh -p ~/Documents/ls.esr
```

. The option `-p` tells the Shell Recorder to keep a protocol file even if none of the test fail. The Shell Recorder prints the location of
the protocol file in it’s output:

```
…
📕
Protocol File: /var/folders/hx/flbncdhj4fs87095gzxvnj3h0000gn/T/elektraenv.XXXXXXXXX.MyZLuGKE
```

. If we take a look at the protocol file we see that it contains the the commands from above, together with return values, standard (error)
output, warnings and error values. For example, the last `kdb set` command produced the following text in the protocol file:

```
CMD: kdb set user:/test/ls/the/next/level
RET: 0
STDOUT: Set null value
```

. We can now take the file `ls.esr` and the protocol file to check if running the test a second time produces the same output:

```sh
mv /var/folders/hx/flbncdhj4fs87095gzxvnj3h0000gn/T/elektraenv.XXXXXXXXX.MyZLuGKE ~/Documents/ls.epf
build/tests/shell/shell_recorder/shell_recorder.sh -p ~/Documents/ls.esr ~/Documents/ls.epf
```

. The Shell Recorder then prints the following output if everything went fine:

```
kdb set user:/test/ls/level1 'one'
kdb ls user
kdb set user:/test/ls/level1/level2 'two'
kdb set user:/test/ls/the 'roots'
kdb ls user
kdb set user:/test/ls/the/next/level
=======================================
Replay test succeeded
```

.

### Adding Replay Tests

If you want to add a replay test to the test suite of Elektra, you can do so by moving your test file (`filename.esr`) and your protocol
file (`filename.epf`) to the folder `replay_tests`. The text `filename` specifies the name of the replay test.
