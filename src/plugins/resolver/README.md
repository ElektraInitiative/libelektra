- infos = All information you want to know are in keys below
- infos/author = Markus Raab <elektra@markus-raab.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/placements = rollback getresolver setresolver commit
- infos/needs =
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

The build-in resolving works like (with ~ resolved from system):

- for system and absolute path: path
- for system and relative path: KDB_DB_SYSTEM + path
- for user and absolute path: ~ + path
- for user and relative path: ~ + KDB_DB_USER + path

Many variants might change this build for different variants of the
resolver plugin, typically by using environment variables.

Environment variables are very simple for one-time usage but their
maintenance in start-up scripts is problematic. Additionally, they
are controlled by the user, so they cannot be trusted. So it is not
recommended to use environment variables.

Note that the file permissions apply, so it might be possible for
non-root to modify keys in system.


### XDG Compatibility ###

The resolver is fully [XDG compatible](http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html)
if configured with the variant:

- xp, xh or xu for user (either using passwd, HOME or USER as fallback
  or any combination of these fallbacks)
- x for system, no fallback necessary

Additionally KDB_DB_USER needs to be left unchanged as `.config`.

XDG_CONFIG_DIRS will be used to resolve system pathes the following
way:

- if unset or empty /etc/xdg will be used instead
- all elements are searched in order of importance
 - if a file was found, the search process is stopped
 - if no file was found, the least important element will be used for
   potential write attempts.


## Reading Configuration ##

 1.) stat the file
 2.) check if the file stat has changed
 3.) remember the time (last update)


## Writing Configuration ##


 1.) Open the configuration file
     If not available recursively create directories and retry.
#ifdef ELEKTRA_LOCK_MUTEX
 1.) Try to lock a global mutex, if not possible -> conflict
#endif
#ifdef ELEKTRA_LOCK_FILE
 1.) Try to lock the configuration file, if not possible -> conflict
#endif
 2.) Check the update time -> conflict
 3.) update the update time



#ifdef ELEKTRA_CONFLICT_DEBUG

## Debugging Conflicts ##

You have build the resolver with an option that will send a signal
SIGSTOP during its critical section.

This way you can easily investigate problems.
There is also a shellscript test, which does that.

DO NOT USE THIS PLUGIN IN PRODUCTION!!!

IT IS FOR DEBUG PURPOSES ONLY!!!

#endif

