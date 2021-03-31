# elektra-fuse

Mount [elektra](https://www.libelektra.org)'s KDB as a [fuse(py)](https://github.com/fusepy/fusepy) filesystem.

This tool enables the inspection and modification of the KDB, in the form of a classical filesystem, as it is observed by a given running process.
In the simplest case, an Elektra key appears as a file with the key name as file path and with the key value as file content.

This enables users to interact with Elektra
- using standard unix tooling, making Elektra more accessible
- with standard system/library procedures
- without the need to learn other tooling of libelektra
- with additional features: e.g. the ability to effortlessly write binary keys

## Quickstart

### Docker

Ensure docker is installed and run:
```sh
./run_new_container.sh
```
This (re)creates the container if necessary and starts an interactive session with keys for testing already created (see `docker/create_keys.sh`).
The filesystem is mounted below `~/mount`.
For debugging purposes, a logfile is written to `~/nohup.out` inside the container. 

### Native

Required (debian) packages:
- libfuse-dev
- fuse
- libelektra5-all (not in official repositories, follow [these](https://www.libelektra.org/installation/installation) instructions)

Required (python3) packages:
- fusepy psutil

Mount the filesystem below the (already existing) directory `<mount point>` (as root-user) with
```sh
sudo ./elektra_fuse/elektra_fuse.py <mount point>
```

## Filesystem Structure

Directly below the mount point, for every process running on the system (except those whose current working directory lies below said mountipoint, e.g. the shell exporing the hierarchy), a directory named after the respective process identifier is exposed.

For convinience, each such directory possesses numerous extened filesystem attributes that identify the process, user, etc., that can be listed by:

```sh
xattr -l <mount point>/<pid>
```

Below any such pid-directory, the elektra key database is mounted bidirectionally (i.e. read-write capable) using directories that correspond to the different [namespaces](https://www.libelektra.org/tutorials/namespaces):

- `user:`
- `system:`
- `dir:`
- `spec:`

Exceptions (at the moment, these namespaces behave differently)
- `default:`
- `proc:`

Furthermore, there is `cascading:`, which correspons to the `/` Elektra-namespace.
This is useful for inspection purposes, but beware:
Write operations in this namespace produce unexpected effects (and create operations might fail), as for the same "address" a write operation can be resolved to a different real key then the corresponding read operation.

These first two layers, (e.g `mountpoint/12/system:`) are read only. Deeper layers support writing: any change is directly updated in the backing Elektra-Key-Database.

### Process Context Mocking

All Elektra operations are performed "from the perspective" of the respective process.
This is done by only calling libelektra/kdb from special "mock"-processes that mimic all of the relevant attributes Elektra uses to resolve its keys.

The mocked attributes are:
- the user and group (for the `user:` namespace)
- working directory (for the `dir:` namespace)
- environment variables (for the `proc:` namespace)
- in the future: the process arguments


### Example file system structure

The filesystem structure mounted below the mountpoint could present as follows: (restructed to only one process with pid 41, with a maximum listing depth of 4)

```
|-- 41
|   |-- cascading:
|   |   |-- dir_and_file_at_once
|   |   |   `-- leaf
|   |   |-- dirkey
|   |   |-- elektra
|   |   |   |-- modules
|   |   |   `-- version
|   |   |-- info
|   |   `-- person
|   |       `-- name
|   `-- user:
|       |-- dir_and_file_at_once
|       |   |-- @elektra.value
|       |   `-- leaf
|       |-- info
|       `-- person
|           `-- name

```
as obtained with
```sh
tree -L 4 mountpoint
```



## Assumptions and special behaviour

Elektra cannot map perfectly to the structure of a classical filesystem as described [here](https://www.libelektra.org/docgettingstarted/big-picture).
Whenever deviations are necessary, the goal is to maximize usability and adhere to the expectations of common Unix tools.

### Intermediate directories

In Elektra, there is no need to have a `path` of continous keys between ancestors.
E.g. the existence of `user:/a` and `user:/a/b/c` does not imply the existence of `user:/a/b`.
Such intermediate keys are mapped to "virtual" directories, s.t. navigation in an interactive manner is possible and the directory tree is continous.

A side effect is that creating/deleting a single key can create/delete multiple "virtual" directories at once.
Another is that copies created with `cp -r` will not have these vacuous intermediaries but instead empty keys.

### Keys that act both as file and directory

In Elektra, there are only keys and no distinction between directories and files are made. To enable a bidirectional mapping into a classical file system, the following mechanism is employed:

If a key (`A`) exists, and has some child key (`B`), `A` is treated as a directory containing `B` and a special file `@elektra.value`. This file is used to access the value of `A`.

Some side effects arise:
E.g. recursively deleting directories with `rm -r` containing `@elektra.value` files raises (task uncritical) errors when attempting to delete a parent directory which was already deleted with the removal of an `@elektra.value` file directly below.

### "Leaf"-Keys that act as as directories

Keys at the bottom of the hierarchy are normally treated as files. However in certain cases, like explicitly creating directories using `mkdir`, this would result in files being displayed, making it impossible to expand the hierarchy using standard tools.
Therefore, when a directory is created, the special meta-key `fuse/directory` is set to override this behaviour.

### Filesystem attributes

The filesystem attributes are sourced from the file obtained by `kdb file`.
For some keys, no such file is returned, or the returned file does not exist in actuality. In these cases, fake attributes are used to preserve usability.

To increase compatibility, currently the timestamps from these files are not used, as this prompts software like `vim` to produce warnings/errors.

## Meta-Keys

Meta-keys are mapped bidirectionally to extended file attributes (xattrs).
To enable interoperability with other tools, the `meta:/` prefix is optional when reading/writing accessing meta keys. The listing of attributes however will show them prefix-less.

Furthermore, no binary values for these attributes are allowed. (As libelektra does not accept those).

See `xattr(1)` on how to read/write/list these attributes.

## Binary-Keys

If a value is to be written that cannot be decoded using the systems default encoding, it is treated as binary, and the appropriate `meta:/binary` meta-key is set.

## Known issues

- Moving an ``@elektra.value``-file corrupts the filesystem/key database (See this [issue](https://issues.libelektra.org/3648)). As editors like `vim` may move a file during editing, such editors cannot reliably be used on these pseudo-files.
- Only works on POSIX-compatible systems with support for FUSE (via fusepy)
- chmod/chown is not implemented and does not signal a "not supported error" to enable compatibility with common tools like `cp`.
- Access related errorcodes not yet implemented (e.g. normal user tries to write to system:/) (defaulting to read only fs - error)
- Everything that is labled as `TODO` in the source.
