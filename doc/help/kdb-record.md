# kdb-record(1) -- Session recording for Elektra

## SYNOPSIS

`kdb record <COMMAND>`

Where `<COMMAND>` is one of the following:

- `start [session_name]`: start a new recording session.
- `stop`: stop the active recording session.
- `resume <session_name>`: resume a stopped recording session.
- `reset [session_name]`: remove all data from the recording session.
- `delete <session_name>`: delete the recording session.
- `assert [--meta meta_name] <key_name>`: marks a key to be used for value-based validation during import.
- `list`: list all available recording sessions.
- `name`: prints the name of the currently active recording session.
- `status [session_name]`: shows a human-readable list of changes.
- `export  [--format replay|diff] [session_name]`: creates a file with the changes of the recording session.
- `import [--disable-assertions] <file_name>`: imports files created by the `export` command.

## DESCRIPTION

The `kdb record` utility is used to manage session recordings.
Only a single recording session can be active per user at a time.
A user can have multiple non-active recording sessions.

If a recording session is active, every change to the keys, values and meta-data stored in Elektra will be logged into that recording session.

## ENVIRONMENT VARIABLES

- `ELEKTRA_RECORD_ACTIVE_SESSION`: If this environment variable is set, the specified recording session will be the active recording session.

## COMMON PARAMETERS

Most commands allow specification of the name of the recording session they use.
If no recording session name is provided, the active recording session will be used.

Notable exceptions are the `start`, `resume` and `delete` commands.

## COMMANDS

### `start [session_name]`
The `start` command is used to start a new recording session.
If the environment variable `ELEKTRA_RECORD_ACTIVE_SESSION` is set, this command has no immediate effect.
As soon as the environment variable is unset, the recording session started with this command will be active.

This command will accept a single optional argument that is the name of the recording session.
If the name is not provided, a random name will be generated.

The command will output an error if a recording session with the specified name already exists.

On success, the name of the recording session will be printed to `stdout`, and the return code is `0`.

### `stop`
The `stop` command is used to stop the active recording session.
The recording session can then be resumed using the `resume` command with the appropriate recording session name.
Stopping a recording session will not delete any recorded changes.

### `resume <session_name>`
The `resume` command is used to resume an existing recording session.
If the environment variable `ELEKTRA_RECORD_ACTIVE_SESSION` is set, this command has no immediate effect.
As soon as the environment variable is unset, the recording session resumed with this command will be active.

This command will accept a single required argument that is the name of the recording session.
If the name is not provided, or a recording session with the specified name does not exist, the command will output an error.

On success, the name of the recording session will be printed to `stdout`, and the return code is `0`.

### `reset [session_name]`
The `reset` command will clear the specified recording.
If no recording session name argument is provided, the currently active recording session will be reset.

### `delete <session_name>`
Deletes the specified recording session.
If it is the currently active recording session, it will be stopped.
If the session does not exist the command will output an error.

### `assert [--meta meta_name] <key_name>`
This command marks a key to be used for validation when importing the recording.
By default, the values of a marked key will be compared when importing.
The import will fail if the key has a different value.

Using the `--meta` flag, one can specify to use the value of a metakey instead of the value of the key.

### `list`
Lists all known recording sessions.

### `name`
Prints out the name of the currently active recording session.
If no recording session is active, the return code is `1`, otherwise `0`.

### `status [session_name]`
Shows all the changes in the specified recording session in human-readable format.
Outputs to `stdout`.
If no recording session is specified, the currently active recording session is used.

### `export  [--format replay|diff] [session_name]`
Exports the changes made in the specified recording session.
Outputs to `stdout`.
If no recording session is specified, the currently active recording session is used.

The `export` command has the ability to output two different types: diffs and replays.

The `--format diff` argument (default) only exports the changes between the start and end states of the recording session.
This means, e.g., that when you add a key and remove it in the same recording session, it will not be exported.

The `--format replay` argument creates a file that contains every modifying operation made in the recording session in the order they were performed.
This is regardless whether at the end of the recording session the value has really changed or is the same as before.
Files created using `--format=replay` serve as a transaction log.


### `import [--disable-assertions] <file_name>`
Imports files created by the `export` command.

By default we check whether the current values in the key database match the "old" values in the files.
The values of keys and meta-keys marked with the `assert` command will also be checked.
Those checks can be disabled using the `--disable-assertions` flag.


## EXAMPLE

```
$ kdb record start AddNewHosts
AddNewHosts
$ kdb record assert system:/hosts/ipv6/localhost
Will assert system:/hosts/ipv6/localhost to be ::1
$ kdb set system:/hosts/ipv4/www.example.com 8.8.8.8
$ kdb set system:/hosts/ipv4/www.beispiel.de 4.4.4.4
$ kdb rm system:/hosts/ipv4/www.ejemplo.es
$ kdb record status
Will assert system:/hosts/ipv6/localhost to be ::1
Changed system:/hosts/ipv4/www.example.com from 1.2.3.4 to 8.8.8.8
Added system:/hosts/ipv4/www.beispiel.de with value 4.4.4.4
Removed system:/hosts/ipv4/www.ejemplo.es
$ kdb record export --format replay
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
$ kdb record import --disable-assertions /tmp/invalid
Successfully imported!
```

## SEE ALSO

- [kdb(1)](kdb.md) 
