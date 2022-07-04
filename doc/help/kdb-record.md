# kdb-record(1) -- Session recording for Elektra

## SYNOPSIS

`kdb record <COMMAND>`

Where `<COMMAND>` is one of the following:

- `start [session_name]`: start or resume a recording session.
- `stop`: stop the active recording session.
- `reset [session_name]`: remove all logged data from the recording session.
- `delete [session_name]`: delete the recording session.
- `mark [--meta meta_name] <key_name>`: marks a key to be used for validation.
- `list`: list all available recording sessions.
- `name`: prints the name of the currently active session.
- `changes [session_name]`: shows a human-readable list of changes.
- `export  [--replay|--diff] [session_name]`: creates a diff of all the changes in the session.
- `import [--disable-validation] <file_name>`: imports files created by the `export` command.

## DESCRIPTION

The `kdb record` utility is used to manage session recordings.
Only a single recording session can be active per user at a time.
A user can have multiple non-active sessions.

If a session is active, every change to the keys, values and meta-data stored in Elektra will be logged into that session.

## ENVIRONMENT VARIABLES

- `ELEKTRA_RECORD_ACTIVE_SESSION`: If this environment variable is set, the specified session will be the active session.

## COMMON PARAMETERS

Most commands allow specification of the name of the session they should work against.
If no session name is provided, the active session will be used.

A notable exception is the `start` command. 
If no session name is provided, it will create a random name.

## COMMANDS

### `start [session_name]`
The `start` command is used to start a new or resume an existing session.
If the environment variable `ELEKTRA_RECORD_ACTIVE_SESSION` is set, this command has no immediate effect.
As soon as the environment variable is unset, the session started or resumed with this command will be active.

This command will accept a single optional argument that is the name of the session. 
If the name is not provided, a random name will be generated. 

On success, the name of the session will be printed to `stdout`, and the return code is `0`.

### `stop`
The `stop` command is used to stop the active recording session. 
The session can then be resumed using the `start` command with the appropriate session name.
Stopping a recording session will not delete any recorded changes.

### `reset [session_name]`
The `reset` command will clear the specified recording. 
If no session name argument is provided, the currently active session will be reset.

### `delete [session_name]`
Deletes the specified recording session. 
It the optional argument is not provided, the current session will be deleted.
If it is the currently active session, it will be stopped.

### `mark [--meta meta_name] <key_name>`
This command marks a key to be used for validation when importing the recording.
By default, the values of a marked key will be compared when importing.
The import will fail if the key has a different value.

Using the `--meta` flag, one can specify to use the value of a specific meta instead of the value of the key.

### `list`
Lists all known recording sessions.

### `name`
Prints out the name of the currently active recording session.
If no session is active, the return code is `1`, otherwise `0`.

### `changes [session_name]`
Shows all the changes in the specified session in human-readable format.
Outputs to `stdout`.
If no session is specified, the currently active session is used.

### `export  [--replay|--diff] [session_name]`
Exports the changes made in the specified session.
Outputs to `stdout`.
If no session is specified, the currently active session is used.

The `export` command has the ability to output two different types: diffs and replays.

The `--diff` flag (default) only exports what has actually changed in the recording session.
This means, e.g., that when you add a key and remove it in the same session, it will not be exported.

The `--replay` flag creates a file that contains every modifying operation made in the session in the order they were performed.
This is regardless whether at the end of the session the value has really changed or is the same as before.
Files created using `--replay` serve as a transaction log.


### `import [--disable-validation] <file_name>`
Imports files created by the `export` command.

By default we check whether the current values in the key database match the "old" values in the files.
The values of marked keys will also be checked.
Those checks can be disabled using the `--disable-validation` flag.


## EXAMPLE

```
$ kdb record start AddNewHosts
AddNewHosts
$ kdb record mark system:/hosts/ipv6/localhost
Marked system:/hosts/ipv6/localhost to be ::1
$ kdb set system:/hosts/ipv4/www.example.com 8.8.8.8
$ kdb set system:/hosts/ipv4/www.beispiel.de 4.4.4.4
$ kdb rm system:/hosts/ipv4/www.ejemplo.es
$ kdb record changes
Marked system:/hosts/ipv6/localhost to be ::1
Changed system:/hosts/ipv4/www.example.com from 1.2.3.4 to 8.8.8.8
Added system:/hosts/ipv4/www.beispiel.de with value 4.4.4.4
Removed system:/hosts/ipv4/www.ejemplo.es
$ kdb record export --replay
<export output. format not yet defined>
$ kdb record list
AddNewHosts
SomeOtherSession
$ kdb record name
AddNewHosts
$ ELEKTRA_RECORD_ACTIVE_SESSION=JustATest kdb record name
JustATest
$ kdb record reset
$ kdb record changes
<no changes>
$ kdb record import /tmp/invalid
Error importing! 
Existing value for key system:/hosts/ipv6/localhost does not match! (shoud be ::1, is ::2)
$ kdb record import --disable-validation /tmp/invalid
Successfully imported!
```

## SEE ALSO

- [kdb(1)](kdb.md) 