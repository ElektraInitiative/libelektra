# How-to: Add external commands

This tutorial will describe how to provide `kdb` with the specification of external programs.
So `kdb` can parse and check the provided options and arguments according to the provided specification.
This allows you to have, for example, a shell script but its args are checked by `kdb` before running it.
It is possible to either mount(`kdb mount`) the specification, or set the keys manually using `kdb set` and `kdb meta-set`.
Both options will be described in the following.
Here we'll define the specification for a simple script that removes files by moving them to a trash folder instead of directly deleting them.

Since the arguments are already checked by `kdb`, the script knows these two are true

1. The script can assume that if `$#` is `1`, `$1` has to be the file.
2. And if `$#` is not `1` it hast to be `2`, with `$1` being the `-f` flag and `$2` being the filename.

because the spec won't allow anything else.

```bash
#!/bin/bash

if [ $# -eq 1 ]; then
    # 1. was only called with the filename
    mkdir -p $HOME/.trash

    mv $1 $HOME/.trash
    echo moved $1 to trash
else
    # 2. was called with -f flag and the filename
    rm -f $2
    echo deleted $2
fi
```

Calling `kdb --help` will contain:

```bash
Usage: kdb [OPTION...] [COMMAND [...]]

OPTIONS
  --help                      Print this help message

COMMANDS
  ...
  ...
  trash                       Move a file to trash
  ...
  ...
```

and calling `kdb trash --help` will result in:

```bash
Usage: kdb trash [OPTION...] <file>

OPTIONS
  --help                      Print this help message
  -f, --force                 Delete the file directly

PARAMETERS
  file                        the file that shall be deleted
```

For a reference of how the specification can look like [Command Line Options](command-line-options.md).

## With `kdb mount`

```ni
[file]
 meta:/description = the file that shall be deleted
 meta:/args = indexed
 meta:/args/index = 0

[force]
 meta:/description = Delete the file directly
 meta:/opt = f
 meta:/opt/long = force
 meta:/opt/arg = none

[]
 meta:/command = trash
 meta:/description = Move a file to trash
 meta:/external = 1
 meta:/bin = /path/to/trash.sh
```

The file then has to be mounted with

```sh
kdb mount /path/to/spec.ni spec:/sw/elektra/kdb/#0/current/trash mini
```

## Alternative to `kdb mount`

This is the same as mounting the spec file.

```bash
kdb set spec:/sw/elektra/kdb/#0/current/trash ""
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash external 1
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash bin "/path/to/trash.sh"
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash command "trash"
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash description "Move a file to trash"
kdb set spec:/sw/elektra/kdb/#0/current/trash/file ""
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/file description "The file that should be moved to trash"
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/file args indexed
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/file args/index 0
kdb set spec:/sw/elektra/kdb/#0/current/trash/force ""
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/force description "Delete the file directly"
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/force opt f
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/force opt/long force
kdb meta-set spec:/sw/elektra/kdb/#0/current/trash/force opt/arg none
```

> **_NOTE:_** Extra arguments are directly passed on to the external command. So it is possible to provide the external program with more
> args than specified in the spec. Those are not check by `KDB`.

So basically keys in `spec:/sw/elektra/kdb/#0/current/..` are considered external commands as long as the metakey `external` is set to 1
and a metakey `bin`, that has the path to the binary, is set. Instead of mounting the spec file it is also possible to the set spec
manually using `kdb`.

`bin` should be an absolut path. If it is not, the binary will be search relative to where `kdb` is executed.

External commands specified like this will appear in `kdb --help` and can be used with `kdb <command>` like any other command.
