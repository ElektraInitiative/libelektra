- infos = All information you want to know are in keys below
- infos/author = Markus Raab <elektra@markus-raab.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/needs =
- infos/placements = rollback getresolver setresolver commit
- infos/status = productive maintained specific unittest tested libc configurable
- infos/description =
## Scope ##

The @PLUGIN_SHORT_NAME@ handles operating system dependent tasks.
One task is the resolving of the filenames for user and system (hence its name)


We have an optimistic approach. Locking is only used to detect
concurrent cooperative processes in the short moment between prepare and commit.
A conflict will be raised in that situation.
When processes do not lock the file it might be overwritten.
This is unavoidable because
such problems can only be detected in the commit phase when it is too late for
rollbacks.

## Resolving Files ##

Use following command to see to which file is resolved to:

    kdb file <Elektra path you are interested in>

See the constants of this plugin for further information, that are:

    system/elektra/modules/resolver/constants
    system/elektra/modules/resolver/constants/ELEKTRA_VARIANT_SYSTEM
    system/elektra/modules/resolver/constants/ELEKTRA_VARIANT_USER
    system/elektra/modules/resolver/constants/KDB_DB_HOME
    system/elektra/modules/resolver/constants/KDB_DB_SYSTEM
    system/elektra/modules/resolver/constants/KDB_DB_USER
    system/elektra/modules/resolver/constants/KDB_DB_SPEC
    system/elektra/modules/resolver/constants/KDB_DB_DIR

The build-in resolving works like (with ~ and `pwd` resolved from system):

- for spec with absolute path: path
- for spec with relative path: KDB_DB_SPEC + path
- for dir with absolute path: `pwd` + path (or above when path is found)
- for dir with relative path: `pwd` + KDB_DB_DIR + path (or above when path is found)
- for user with absolute path: ~ + path
- for user with relative path: ~ + KDB_DB_USER + path
- for system with absolute path: path
- for system with relative path: KDB_DB_SYSTEM + path


## Example

For an absolute path /example.ini, you might get following values:

- for spec: /example.ini
- for dir: `pwd`/example.ini
- for user: ~/example.ini
- for system: /example.ini


For an relative path example.ini, you might get following values:

- for spec: /usr/share/elektra/specification/example.ini
- for dir: `pwd`/.dir/example.ini
- for user: ~/.config/example.ini
- for system: /etc/kdb/example.ini


## Variants ##

Many variants exist that additionally influence the lookup
process, typically by using environment variables.

Environment variables are very simple for one-time usage but their
maintenance in start-up scripts is problematic. Additionally, they
are controlled by the user, so they cannot be trusted. So it is not
recommended to use environment variables.

Note that the file permissions apply, so it might be possible for
non-root to modify keys in system.

See [COMPILE.md](/doc/COMPILE.md) for a documentation of possible
variants.


### XDG Compatibility ###

The resolver is fully [XDG compatible](http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html)
if configured with the variant:

- xp, xh or xu for user (either using passwd, HOME or USER as fallback
  or any combination of these fallbacks)
- x for system, no fallback necessary

Additionally KDB_DB_USER needs to be left unchanged as `.config`.

XDG_CONFIG_DIRS will be used to resolve system paths the following
way:

- if unset or empty /etc/xdg will be used instead
- all elements are searched in order of importance
 - if a file was found, the search process is stopped
 - if no file was found, the least important element will be used for
   potential write attempts.


## Reading Configuration ##

 1.) If no update needed (unchanged modification time): ABORT
 2.) remember the last stat time (last update)


## Writing Configuration ##

 0.) On empty configuration: remove the configuration file and ABORT
 1.) Open the configuration file
     If not available recursively create directories and retry.
#ifdef ELEKTRA_LOCK_MUTEX
 1.) Try to lock a global mutex, if not possible -> conflict
#endif
#ifdef ELEKTRA_LOCK_FILE
 1.) Try to lock the configuration file, if not possible -> conflict
#endif
 2.) Check the update time -> conflict
 3.) Update the update time (in order to not self-conflict)
