- infos = Information about the ansible plugin is in keys below
- infos/author = Maximilian Irlinger <max@maxirlinger.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ansible
- infos/recommends =
- infos/placements = setstorage
- infos/status = recommended productive maintained conformant
- infos/metadata =
- infos/description = export Ansible playbooks

## Introduction

Provides a write-only storage plugin for use with `kdb export` and `kdb record-export`.
The output format is an Ansible playbook that utilized the [ansible-libelektra](https://github.com/ElektraInitiative/ansible-libelektra) module.

## Plugin Configuration

You can use the following configuration keys to modify the behavior and output of the plugin:

| Key              | Default Value       | Description                                                      |
| :--------------- | :------------------ | :--------------------------------------------------------------- |
| `playbook`       | `true`              | Whether to generate a whole playbook or just a (list of) task(s) |
| `playbook/name`  | My Elektra Playbook | The `name` property of the playbook                              |
| `playbook/hosts` | all                 | The `hosts` property of the playbook                             |
| `task/main/name` | Set Elektra Keys    | The name of the 'main' task of the playbook                      |

## Dependencies

This plugin requires [yaml-cpp][]. On a Debian based OS the package for the library is called [`libyaml-cpp-dev`](https://packages.debian.org/libyaml-cpp-dev). On macOS you can install the package [`yaml-cpp`](https://repology.org/project/yaml-cpp) via [HomeBrew](https://brew.sh).

## Examples

```sh
# Backup-and-Restore: user:/tests/ansible

kdb set user:/company/roles/ceo Hans
#> Create a new key user:/company/roles/ceo with string "Hans"

kdb export user:/company ansible -c playbook/name="Company Roles",task/main/name="I can customize this too"
# RET:0
```

```yaml
---
- name: Company Roles
  hosts: all
  collections:
    - elektra_initiative.libelektra
  tasks:
    - name: I can customize this too
      elektra:
        keys:
          - user:
              company:
                roles:
                  ceo:
                    - value: Hans
```

## Limitations

- This plugin only supports writing of Ansible Playbooks.
  It is currently not possible to read them with this plugin.
  Do not use this plugin as a general-purpose storage plugin.

- If keys below `system:/elektra/mountpoints` are included, we will always create a second task for them instead of using the `mount` operation of the ansible-libelektra module.
  This task will be created first.
  Keys created during the 'main' task will then already be able to use the correctly mounted files.

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
