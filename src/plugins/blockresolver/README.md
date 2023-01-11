- infos = Information about the blockresolver plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/recommends =
- infos/placements = rollback getresolver setresolver commit
- infos/status = tested/unit nodep configurable experimental concept
- infos/metadata =
- infos/description = resolver for parts in a file configuration file

## Introduction

The `blockresolver` can be used to only resolve a tagged block inside a configuration file.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

### Implementation Details

`blockresolver` extracts the requested block from the configurations file and writes it into a temporary file. Afterwards Elektra will only work on the temporary file until kdbSet is called. On kdbSet the contents of the temporary file will be merged with parts outside of the requested block from the original file.

## Usage

    `kdb mount -R blockresolver /path/to/my/file /mountpoint -c identifier="identifier-tag"`

where `identifier` specifies the tag `blockresolver` will search for in the configuration file.

A block consists of 2 parts:

- beginning: the identifier suffixed with `start`
- end: the identifier suffixed with `stop`

## Limitations

Currently the identifier must be unique.

## Example

```sh
# Backup-and-Restore: system:/tests/blockresolver

# create testfile
kdb set system:/tests/blockfile $(mktemp)
echo 'text'                   >  $(kdb get system:/tests/blockfile)
echo 'more text'              >> $(kdb get system:/tests/blockfile)
echo 'some more text'         >> $(kdb get system:/tests/blockfile)
echo '>>> block config start' >> $(kdb get system:/tests/blockfile)
echo 'key1=val1'            >> $(kdb get system:/tests/blockfile)
echo '>>> block config stop'  >> $(kdb get system:/tests/blockfile)
echo 'text again'             >> $(kdb get system:/tests/blockfile)
echo 'and more text'          >> $(kdb get system:/tests/blockfile)
echo 'text'                   >> $(kdb get system:/tests/blockfile)

sudo kdb mount -R blockresolver $(kdb get system:/tests/blockfile) system:/tests/blockresolver -c identifier=">>> block config" mini

# check testfile
cat $(kdb get system:/tests/blockfile)
#> text
#> more text
#> some more text
#> >>> block config start
#> key1=val1
#> >>> block config stop
#> text again
#> and more text
#> text

# only the block between the tags is read!
kdb export system:/tests/blockresolver mini
# STDOUT-REGEX: key1.*=.*val1

# add a new key to the resolved block
kdb set system:/tests/blockresolver/key12 val12

cat $(kdb get system:/tests/blockfile)
#> text
#> more text
#> some more text
#> >>> block config start
#> key1=val1
#> key12=val12
#> >>> block config stop
#> text again
#> and more text
#> text

# cleanup
kdb rm -r system:/tests/blockresolver
rm $(kdb get system:/tests/blockfile)
kdb rm system:/tests/blockfile
sudo kdb umount system:/tests/blockresolver
```
