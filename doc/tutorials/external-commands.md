# How-to: Add external commands

For a reference of how the specification can look like [Command Line Options](command-line-options.md).

## With `kdb mount`

```ni
[file]
 meta:/description = the file that shall be deleted
 meta:/args = indexed
 meta:/args/index = 0

[]
 meta:/command = somecommand
 meta:/description = Simple script
 meta:/external = 1
 meta:/bin = /path/to/test.sh
```

The file then has to be mounted with

```sh
kdb mount /path/to/spec.ni spec:/sw/elektra/kdb/#0/current/somecommand mini
```

## Alternative to `kdb mount`

This is the same as mounting the spec file.

```bash
kdb set spec:/sw/elektra/kdb/#0/current/somecommand ""
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand external 1
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand bin "/path/to/test.sh"
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand command "somecommand"
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand description "Simple script"
kdb set spec:/sw/elektra/kdb/#0/current/somecommand ""
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand/file description "the file that shall be deleted"
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand/file args indexed
kdb meta-set spec:/sw/elektra/kdb/#0/current/somecommand/file args/index 0
```

> **_NOTE:_** Extra arguments are directly passed on to the external command. So it is possible to provide the external program with more
> args than specified in the spec. Those are not check by `KDB`.

So basically keys in `spec:/sw/elektra/kdb/#0/current/..` are considered external commands as long as the metakey `external` is set to 1
and a metakey `bin`, that has the path to the binary, is set. Instead of mounting the spec file it is also possible to the set spec
manually using `kdb`.

`bin` should be an absolut path. If it is not, the binary will be search relative to where `kdb` is executed.

External commands specified like this will appear in `kdb --help` and can be used with `kdb somecommand`.
