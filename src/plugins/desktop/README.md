- infos = Information about the desktop plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage info
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended maintained specific nodep libc unfinished nodoc concept
- infos/metadata =
- infos/description =

## Usage ##

To mount the plugin please use:

    bin/kdb mount --resolver noresolver none system/info/desktop desktop

or it is already included if you already mounted the info plugins with:

    kdb mount-info

Then you can get desktop information via:

    kdb get system/info/desktop

You either get a *lower-case* string (supported desktops see below)
or no key if no desktop was detected.


## Purpose ##

The plugin is informational and mainly be used to provide context for
other other configuration. See [elektrify-getenv](src/libs/getenv/README.md).


## Supported Desktop ##

Currently supported desktops are:

- GNOME
- KDE
- TDE
- Unity
- XDG conformant desktops (`XDG_CURRENT_DESKTOP`)

Currently the detection relies on environment variables,
which will not work in setuid or otherwise secured binaries.
Please open a bug report if the detection does not work for you:
http://libelektra.org

