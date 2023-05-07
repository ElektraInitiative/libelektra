# Recording Changes to the KDB

Elektra provides a powerful session recording feature.
You can control it through the `kdb` command-line utility.

Session recording will track all the changes done to the KDB while it is enabled.
This includes changes done by elektrified applications themselves too, not only changes done through the `kdb` utility.
The recorded changes can be exported, undone or used for auditing purposes.

There are seven commands to interact with the session recording feature:

- `kdb record-start`: starts recording.
  If there are existing recorded changes, they will NOT be removed.
  New changes will be appended.
- `kdb record-stop`: stops recording.
- `kdb record-reset`: removes all recorded changes.
- `kdb record-export`: export the changes.
- `kdb record-undo`: undo everything that has been recorded.
- `kdb record-state`: show information about the state of session recording.
  This includes information about what keys will be recorded and which changes have already been made to the KDB.
- `kdb record-rm`: Remove specific keys from the recording.

For a more detailed description and options for each command, please take a look at their respective man pages.

## A Simple Recording Session

```sh
# Enable session recording for all keys below user:/
kdb record-start user:/
# RET:0

kdb set user:/test/name Hans

# View the current state of session recording
kdb record-state
#> Recording is active for user:/⏎⏎Added 1 key(s)⏎Modified 0 key(s)⏎Removed 0 key(s)⏎⏎Added key user:/test/name

# Stop session recording again
kdb record-stop
# RET:0
```

## Exporting Recorded changes

One of the most powerful features of session recording is the ability to export the recorded changes.
This allows you to apply the specific modifications on other computers, without having to export and overwrite the complete configuration.

```
kdb record-export [<source>] [<format>]
```

Where `source` and `format` are optional parameters.
The `source` parameter lets you specify which keys you want to export.
By default, this is all keys, or `/`.
The `format` parameter lets you specify which format you want to export the changes in.
By default, we use the `ansible` format.
Note that the format is just the name of the Elektra storage plugin you want to use.

You can also use the `-c` option to specify a list of configuration parameters for the storage plugin.
See the README of the plugin for more details about which parameters are supported.

```sh
# Export recorded changes into an Ansible playbook
kdb record-export / ansible -c playbook/name="Recording Tutorial"
# RET:0
```

```yaml
---
- name: Recording Tutorial
  hosts: all
  collections:
    - elektra_initiative.libelektra
  tasks:
    - name: Set Elektra Keys
      elektra:
        keys:
          - user:
              test:
                name:
                  - value: Hans
```
