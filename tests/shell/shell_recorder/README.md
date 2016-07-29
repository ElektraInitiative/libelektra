# Shell Recorder #

## Configuration ##

### Mountpoint: ###

This is the only configuration Variable that has to be set. It is used to determine where the shell_recorder should look for changes.
e.g. `Mountpoint: user/test` tells the shell_recorder that you will be working under `user/test`.

### DiffType: ###

Available:

`Dump`, `Ini` exports the keys below `Mountpoint` using either the dump or ini format and diffs the changes.
`File` does a diff on the configuration file mounted to `Mountpoint`.

### File: ###

Tells the shell_recorder what file to use for diffs.
If File is preset but empty a fresh database temp file will be provided for every run.


## Checks ##

Posix-extended regex is used to check and validate return values and outputs.

**Remark:** Shell Recorder uses the `⏎` symbol as line terminator. This means that you need to use the character `⏎`  (instead of `\n`) if you want to match a line ending in a multiline output. For example: Assume there are exactly two keys with the name `key1` and `key2` located under the path `user/test`. The output of the command `kdb ls user/test` would then be the following

```
user/test/key1
user/test/key2
```

You can check this exact output in a shell recorder script via the following code:

```
STDOUT: user/test/key1⏎user/test/key2
< ls user/test
```

If you only want to check that `key1` and `key2` are part of the output you can use the regex `key1.*key2` instead:

```
STDOUT: key1.*key2
< ls user/test
```

As you can see the line ending is considered  a normal character (`.`) in the output.

Options:

* STDOUT:
* STDERR:
* RET:
* WARNINGS:
* ERRORS:
* DIFF:

## Commands ##

A command starts with `<` followed by the usual kdb commands.


## Examples ##

### Basic ###

This testcase uses the dump-format to validate database changes. the first DIFF tells the shell recorder to watch for the line `> keyNew.*user/test/key3` in the diff of the config files after the command `kdb set /test/key val` is executed. The second DIFF + set work the same, but, because this time the line `> keyNew.*user/test/key3` can't be found in the diff, yields an error
```
 % cat db_changes.dat

DiffType: Dump
Mountpoint: user

DIFF: (> keyNew.*user/test/key)
< set /test/key val

DIFF: (> keyNew.*user/test/key3)
< set /test/key2 val2

 % ./shell_recorder.sh db_changes.dat
set /test/key val
set /test/key2 val2
Changes to /home/thomas/.config/default.ecf don't match (> keyNew.*user/test/key3)

 % cat protocol
CMD: set /test/key val
RET: 0
STDERR:
STDOUT: Using name user/test/key
Create a new key user/test/key with string val
WARNINGS:
ERRORS:
DIFF: 2c2,5
< ksNew 0
---
> ksNew 1
> keyNew 14 4
> user/test/keyval
> keyEnd

CMD: set /test/key2 val2
RET: 0
STDERR:
STDOUT: Using name user/test/key2
Create a new key user/test/key2 with string val2
WARNINGS:
ERRORS:
DIFF: 2c2
< ksNew 1
---
> ksNew 2
4a5,7
> keyEnd
> keyNew 15 5
> user/test/key2val2
=== FAILED changes to database file () don't match (> keyNew.*user/test/key3)
```


### Replay ###

```
 % cat old_protocol
CMD: set /tmount/key $RANDOM
RET: 0
STDERR:
STDOUT: Using name user/tmount/key
Create a new key user/tmount/key with string 31371
WARNINGS:
ERRORS:
DIFF: 2,5c2
< ksNew 2
< keyNew 16 6
< user/tmount/key31371
< keyEnd
---
> ksNew 1


 % cat replay_test_fail.dat
Mountpoint: /tmount
DiffType: File

< set $Mountpoint/key $RANDOM


 % ./shell_recorder.sh replay_test_fail.dat old_protocol
set /tmount/key $RANDOM
=======================================
Replay test failed, protocols differ
5c5
< Create a new key user/tmount/key with string 31371
---
> Create a new key user/tmount/key with string 26223
11c11
< < user/tmount/key31371
---
> < user/tmount/key26223

```

### another example ###

```
 % cat script.dat
Storage: dump
Mountpoint: system/testmount/test
File: /tmp/test.dump
MountArgs:
DiffType: File

< set $Mountpoint/test bla

RET: 0
< mount $File $Mountpoint $Storage

RET: 2
< set $Mountpoint teststring

< set $Mountpoint/testkey testval

< get $Mountpoint/testkey

STDOUT: test.?ey
STDERR: blaaaa
< ls $Mountpoint

< setmeta $Mountpoint/testkey metacomment "comment blaa"

< get $Mountpoint/michgibtsgarnicht

WARNINGS: 79|80
< umount $Mountpoint




 % ./shell_recorder.sh script.dat
set system/testmount/test/test bla
mount /tmp/test.dump system/testmount/test dump
set system/testmount/test teststring
Return value 0 doesn't match 2
set system/testmount/test/testkey testval
get system/testmount/test/testkey
ls system/testmount/test
STDERR doesn't match blaaaa
setmeta system/testmount/test/testkey metacomment "comment blaa"
get system/testmount/test/michgibtsgarnicht
umount system/testmount/test
WARNINGS doesn't match 79|80




 % cat protocol
CMD: set system/testmount/test/test bla
RET: 0
STDERR:
STDOUT: Create a new key system/testmount/test/test with string bla
WARNINGS:
ERRORS:
DIFF:

CMD: mount /tmp/test.dump system/testmount/test dump
RET: 0
STDERR:
STDOUT:
WARNINGS:
ERRORS:
DIFF:

CMD: set system/testmount/test teststring
RET: 0
=== FAILED return value doesn't match expected pattern 2
STDERR:
STDOUT: Create a new key system/testmount/test with string teststring
WARNINGS:
ERRORS:
DIFF: 1,6d0
< kdbOpen 1
< ksNew 1
< keyNew 22 11
< system/testmount/testteststring
< keyEnd
< ksEnd

CMD: set system/testmount/test/testkey testval
RET: 0
STDERR:
STDOUT: Create a new key system/testmount/test/testkey with string testval
WARNINGS:
ERRORS:
DIFF: 2c2
< ksNew 2
---
> ksNew 1
5,7d4
< keyEnd
< keyNew 30 8
< system/testmount/test/testkeytestval

CMD: get system/testmount/test/testkey
RET: 0
STDERR:
STDOUT: testval
WARNINGS:
ERRORS:
DIFF:

CMD: ls system/testmount/test
RET: 0
STDERR:
=== FAILED stderr doesn't match expected patter blaaaa
STDOUT: system/testmount/test
system/testmount/test/testkey
WARNINGS:
ERRORS:
DIFF:

CMD: setmeta system/testmount/test/testkey metacomment "comment blaa"
RET: 0
STDERR:
STDOUT:
WARNINGS:
ERRORS:
DIFF: 8,9d7
< keyMeta 12 13
< metacommentcomment blaa

CMD: get system/testmount/test/michgibtsgarnicht
RET: 1
STDERR: Did not find key
STDOUT:
WARNINGS:
ERRORS:
DIFF:

CMD: umount system/testmount/test
RET: 0
STDERR:
STDOUT:
WARNINGS:
=== FAILED Warnings don't match expected pattern 79|80
ERRORS:
DIFF:

```
