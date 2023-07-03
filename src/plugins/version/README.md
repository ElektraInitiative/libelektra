- infos = Information about the version plugin is in keys below
- infos/author = Vid Leskovar <vid.leskovar5@gmail.com>, Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = version
- infos/recommends =
- infos/placements = backend
- infos/status = unittest nodep nodoc
- infos/metadata =
- infos/description = Provides version information for this Elektra installation.

## Introduction

This plugin is very simple.
It implements the minimum functionality required to qualify as a read-only backend plugin.

The data this plugin returns during `kdbGet()` is simple the data returned by `elektraVersionKeySet()` with each `Key` additionally marked with the metadata `restrict/write` and `restrict/remove`.

For version `0.9.4` of Elektra, this plugin returned this data for example:

```ini
system:/elektra/version = "Below are version information of the Elektra Library you are currently using"
system:/elektra/version/constants =
system:/elektra/version/constants/KDB_VERSION = "0.9.4"
system:/elektra/version/constants/KDB_VERSION_MAJOR = "0"
system:/elektra/version/constants/KDB_VERSION_MINOR = "9"
system:/elektra/version/constants/KDB_VERSION_PATCH = "4"
system:/elektra/version/constants/SO_VERSION = "5"
system:/elektra/version/infos = "All information you want to know"
system:/elektra/version/infos/author = "Markus Raab <elektra@markus-raab.org>"
system:/elektra/version/infos/licence = "BSD"
system:/elektra/version/infos/description = "Information of your Elektra Installation"
system:/elektra/version/infos/version = "1"
```
