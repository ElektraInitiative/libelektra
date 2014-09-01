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

## Reading Configuration ##

 1.) stat the file
 2.) check if the file stat has changed
 3.) remember the time (last update)


## Writing Configuration ##


 1.) Open the configuration file
     If not available recursively create directories and retry.
 1.) Try to lock the configuration file, if not possible -> conflict
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




