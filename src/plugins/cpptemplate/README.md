- infos = Information about the cpptemplate plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/info
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep experimental concept
- infos/metadata =
- infos/description = A template for C++ based plugins

# CPP Template

## Introduction

Please use the script [copy-template](../../../scripts/dev/copy-template) to create a new C++ plugin based on this template:

```bash
scripts/dev/copy-template -p pluginname
```

. For more information please take a look [here](../template/README.md)

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Example

```sh
sudo kdb mount -R noresolver none user:/tests/cpptemplate cpptemplate some=thing config=value

# This example plugin adds configuration values at the mount point
kdb ls user:/tests/cpptemplate
#> user:/tests/cpptemplate/%
#> user:/tests/cpptemplate/config
#> user:/tests/cpptemplate/some

kdb get user:/tests/cpptemplate/config
#> value

sudo kdb umount user:/tests/cpptemplate
```
