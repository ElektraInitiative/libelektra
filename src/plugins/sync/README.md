- infos = Information about the sync plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = sync
- infos/needs =
- infos/placements = precommit
- infos/status = recommended productive maintained nodep
- infos/description = Makes sure that config file is written to disc

## Introduction

Storage plugins usually do not use fsync (2) for writing their file
to disc. That means that the content of the file could be in some
buffer and not on the hard disc.

This is good for power saving on laptops, but not a good idea on
production use otherwise. So it is strongly recommended to always
add this plugin.

## Usage

Just add this plugin to the list of plugins during mounting, e.g.

```sh
kdb mount file.dump /important dump sync
```

Then when you observe the change of a value, e.g.

```sh
strace kdb set user:/important/key value
```

you can see, done by storage:

```
open("/home/markus/.kdb/file.dump.16874:1409592592.95084.tmp",
        O_WRONLY|O_CREAT|O_TRUNC, 0666) = 4
write(4, "kdbOpen 1\n", 10)             = 10
write(4, "ksNew 1\n", 8)                = 8
write(4, "keyNew 19 6\n", 12)           = 12
write(4, "user:/important/key\0value\0\n", 26) = 26
write(4, "keyEnd\n", 7)                 = 7
write(4, "ksEnd\n", 6)                  = 6
close(4)                                = 0
```

then done by sync:

```
open("/home/markus/.kdb/file.dump.16874:1409592592.95084.tmp",
        O_RDWR) = 4
fsync(4)                                = 0
close(4)                                = 0
```

and finally commit + sync of directory by resolver:

```
rename("/home/markus/.kdb/file.dump.16874:1409592592.95084.tmp",
        "/home/markus/.kdb/file.dump") = 0
stat("/home/markus/.kdb/file.dump", {st_mode=S_IFREG|0644,
        st_size=69, ...}) = 0
fcntl(3, F_SETLK, {type=F_UNLCK, whence=SEEK_SET, start=0,
        len=0}) = 0
close(3)                                = 0
open("/home/markus/.kdb",
        O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
fsync(3)                                = 0
```
