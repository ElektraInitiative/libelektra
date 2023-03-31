# kdb-record(1) -- Session recording for Elektra

## SYNOPSIS

`kdb record-<COMMAND>`

Where `<COMMAND>` is one of the following:

- `start [--parent key]`: start recording. Will not clear existing recording data.
- `stop`: stop recording.
- `clear`: clear existing recording data.

- `assert [--meta meta_name] <key_name>`: marks a key to be used for value-based validation during import.
- `remove-assert <key_name>`: unmarks a key to be used for value-based validation during import.

- `export <commands|ansible>`: outputs the changes in the specified format.
  - `commands`: a list of `kdb` commands to reproduce the current recorded state
  - `ansible`: an Ansible playbook to reproduce the current recorded state
