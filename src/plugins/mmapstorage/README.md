- infos = Information about the mmapstorage plugin is in keys below
- infos/author = Mihael Pranjić <mpranj@limun.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/mmapstorage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = unittest experimental
- infos/metadata =
- infos/description = high performance storage using memory mapped files

## Introduction

This is a high performance storage plugin that supports full Elektra semantics.

## Format

The storage format uses Elektra's in-memory data layout and employs the `mmap()` system call to read/write data.
The format is not portable across different architectures/platforms. The format can be seen as a memory dump of a keyset.
Therefore, the files must not be edited by hand. Files written by mmapstorage are not intended to be human-readable.

## Usage

Mount mmapstorage using `kdb mount`:

```sh
sudo kdb mount config.mmap user:/tests/mmapstorage mmapstorage
```

Unmount mmapstorage using `kdb umount`:

```sh
sudo kdb umount user:/tests/mmapstorage
```

## Compiling

The mmapstorage has two compilation variants:

1. mmapstorage
2. mmapstorage_crc

The `mmapstorage` will always be compiled on a supported system (see [Dependencies](#dependencies)). When zlib is available,
we will additionally compile the `mmapstorage_crc` variant. The first variant does not do a CRC32 checksum of the critical data,
while the second variant always checks the CRC32 checksum for additional security.

## Installation

See [installation](/doc/INSTALL.md).
The `mmapstorage` variant is part of the `libelektra5` package and the `mmapstorage_crc` is part of the `libelektra5-extra` package.

## Dependencies

POSIX compliant system (including XSI extensions).

Additionally, zlib is needed for the `mmapstorage_crc` compilation variant: `zlib1g-dev` or `zlib-devel`.

## Examples

```sh
# Mount mmapstorage to `user:/tests/mmapstorage`
sudo kdb mount config.mmap user:/tests/mmapstorage mmapstorage

# Add some values via `kdb set`
kdb set user:/tests/mmapstorage 'Some root key'
kdb set user:/tests/mmapstorage/dir 'Directory within the hierarchy.'
kdb set user:/tests/mmapstorage/dir/leaf 'A leaf node holding some valuable data.'
kdb meta-set  user:/tests/mmapstorage/dir/leaf superMetaKey 'Metadata is supported too.'

# List the configuration tree below `user:/tests/mmapstorage`
kdb ls user:/tests/mmapstorage
#> user:/tests/mmapstorage
#> user:/tests/mmapstorage/dir
#> user:/tests/mmapstorage/dir/leaf

# Retrieve the new values
kdb get user:/tests/mmapstorage
#> Some root key
kdb get user:/tests/mmapstorage/dir
#> Directory within the hierarchy.
kdb get user:/tests/mmapstorage/dir/leaf
#> A leaf node holding some valuable data.
kdb meta-get  user:/tests/mmapstorage/dir/leaf superMetaKey
#> Metadata is supported too.

# Undo modifications to the database
kdb rm -r user:/tests/mmapstorage

# Unmount mmapstorage
sudo kdb umount user:/tests/mmapstorage
```

## Limitations

Mapped files shall not be altered, otherwise the behavior is undefined.

The `mmap()` system call only supports regular files and so does the mmapstorage
plugin with one notable exception: The plugin detects when it is called with the
files `/dev/stdin` and `/dev/stdout` and makes an internal copy. This makes the
plugin compatible with `kdb import` and `kdb export`.
