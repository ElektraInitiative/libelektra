- infos = Information about the cpptemplate plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/info
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained unittest nodep experimental nodoc concept
- infos/metadata =
- infos/description = A template for C++ based plugins

# CPP Template

## Introduction

Please use the script [copy-template](../../../scripts/copy-template) to create a new C++ plugin based on this template:

```bash
scripts/copy-template -p pluginname
```

. For more information please take a look [here](../template/README.md)

## Example

```sh
sudo kdb mount -R noresolver none user/tests/cpptemplate cpptemplate some=thing config=value

# This example plugin adds configuration values at the mount point
kdb ls user/tests/cpptemplate
#> user/tests/cpptemplate/config
#> user/tests/cpptemplate/path
#> user/tests/cpptemplate/some

kdb get user/tests/cpptemplate/config
#> value

kdb get user/tests/cpptemplate/path
#> none

sudo kdb umount user/tests/cpptemplate
```
