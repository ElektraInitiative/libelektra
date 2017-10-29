# Shell Recorder

The Shell Recorder is a test tool that checks the output and return values of  shell commands such as `kdb`. It allows you to write unit and  regression tests. The Shell Recorder also records the result of tests in so-called protocol files. These protocol files can be used as basis for new Shell Recorder tests.

## Configuration

### Mountpoint

This is the only configuration variable that has to be set. It is used to determine where the `shell_recorder` should look for changes.
e.g. `Mountpoint: user/test` tells the `shell_recorder` that you will be working under `user/test`.

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

**Remark:** Shell Recorder uses the `⏎` symbol as line terminator. This means that you need to use the character `⏎`  (instead of `\n`) if you want to match a line ending in a multiline output. For example: Assume there are exactly two keys with the name `key1` and `key2` located under the path `user/test`. The output of the command `kdb ls user/test` would then be the following

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
* `ERRORS:` The Shell Recorder compares this **comma separated list** of numbers with the errors thrown by a `kdb` command.
* `DIFF:` This **regular expression** is compared with the output of a `diff` command. The input for the `diff` command is the last and current state of the key database. The directive `DiffType` specifies if the the Shell Recorder compares the file of the storage plugin (`File`) directly, or if it uses the INI (`Ini`) or Dump (`Dump`) format.

## Commands

A command starts with `<` followed by kdb or shell commands.

## Examples

Please take a look at the examples files (`*.esr`) located inside this folder.
