# `kdb record` Command

The `kdb record` tooling enables the user to record configuration sessions. It also allows exporting and importing the configuration changes.

Recording sessions are stored on a per-user basis. This implies that a recording session can only track changes made by a single user.

## Session handling

* `kdb record start [session_name]`: Starts a new recording session. If no session name is provided, a new random name is generated. If a session with the provided name already exists, changes will be appended.
* `kdb record stop`: Stops the current recording session. 
* `kdb record reset [session_name]`: Removes all recorded changes in this session. If no session name is provided, the current session will be reset.
* `kdb record delete [session_name]`: Deletes the specified recording session. If no session name is provided, the current session will be deleted. Deleting the current session will stop the recording. 
* `kdb record list-sessions`: Lists all sessions by their name
* `kdb record changes [session_name]`: Shows all changes for the specified session. If no session name is provided, the current session will be used. 

## Exporting configuration changes

`kdb record export [session_name] [--format <format>]`: Will dump the changed keys of the specified session onto `stdout`. If no session name is provided, the current session will be used. 

As example, suppose the key `user:/my/app/hello` has the value `world` and you execute the following commands:
* `kdb set user:/my/app/hello max`
* `kdb set user:/my/app/hello susi`

Only the value `susi` will be exported. Additionally, if you execute `kdb set user:/my/app/hello world` the key will not be exported at all, as there are no changes compared to the beginning of the recording.

The same principle applies to the creation and deletion of keys within a session. If you create a key and delete it in the same session, it will not be exported.

By default, the elektra dump format is used. The optional --format parameter can be used to specify which storage plugin should be used to format the output.

## Importing configuration changes

The output of the `kdb record export` command can be imported with the existing `kdb import` functionality. Depending on the specified storage plugin, other steps may be necessary, e.g. the Ansible plugins creates an Ansible playbook.

## Example workflow

The following example changes the IP address of www.google.com, adds an entry for www.microsoft.com and removes www.apple.com. It then shows the changes and outputs an Ansible playbook.

```
$ kdb record start AddNewHosts
$ kdb set system:/hosts/ipv4/www.google.com 8.8.8.8
$ kdb set system:/hosts/ipv4/www.microsoft.com 4.4.4.4
$ kdb rm system:/hosts/ipv4/www.apple.com
$ kdb record stop
$ kdb record changes
Changed system:/hosts/ipv4/www.google.com from 1.2.3.4 to 8.8.8.8
Added system:/hosts/ipv4/www.microsoft.com with value 4.4.4.4
Removed system:/hosts/ipv4/www.apple.com
$ kdb record export --format ansible
<output corresponding to ansible-libelektra plugin (https://galaxy.ansible.com/elektra_initiative/libelektra)>
```

## Architecture

The core of `kdb record` consists of two parts: 
* a global plugin that records changes to the key database
* tooling that
  - allows configuration of the plugin (enable, disable)
  - allows managing recording sessions
  - computes the minimal change set and displays and exports it 

### Session storage

The recording sessions and plugin configuration will be stored in Elektra. As this is a per-user feature, it will go into the `user:/` namespace. As for now, the root key is `user:/record-elektra/`.

The key `user:/record-elektra/config/current_session` contains the name of the current session. If this key does not exist (or its value is empty) the recording plugin is disabled.

All recording sessions are located under `user:/record-elektra/sessions/<SESSION_NAME>`.

Every modification will include its UTC timestamp and the modified key in its path. 
For example, `user:/record-elektra/sessions/MySession/12345678/system:\/hello`. The value of this key will be the modification action, i.e. `created`, `modified`, `deleted`.

Based on which modification took place (creation, modification or deletion), the old and new value for the key will also be recordes as `.../old` and `.../new`.

For the workflow example from below, it would look like this (11111, 22222, 33333 simulate time stamps):
```
user:/record-elektra/sessions/AddNewHosts/111111/system:\/hosts\/ipv4\/www.google.com (= modified)
user:/record-elektra/sessions/AddNewHosts/111111/system:\/hosts\/ipv4\/www.google.com/old (= 1.2.3.4)
user:/record-elektra/sessions/AddNewHosts/111111/system:\/hosts\/ipv4\/www.google.com/new (= 8.8.8.8)
user:/record-elektra/sessions/AddNewHosts/222222/system:\/hosts\/ipv4\/www.microsoft.com (= created)
user:/record-elektra/sessions/AddNewHosts/222222/system:\/hosts\/ipv4\/www.microsoft.com/new (= 4.4.4.4)
user:/record-elektra/sessions/AddNewHosts/333333/system:\/hosts\/ipv4\/www.apple.com (= deleted)
user:/record-elektra/sessions/AddNewHosts/333333/system:\/hosts\/ipv4\/www.apple.com/old (= 9.9.9.9)
``` 

### Recording plugin

When active, the recording plugin needs to be called for every `kdbGet()` and `kdbSet()` operation. 
The plugin needs to track which keys have been read and which have been set. 

The plugin MUST ignore `kdbGet()` and `kdbSet()` invocations for its own keys (`user:/record-electra/`).

The `kdbSet()` function of the plugin has to be called in the `POSTCOMMIT` phase of the key database.

As for the execution of the recording plugin, there are multiple options:
* hardcode the invocation in the `kdbGet()` and `kdbSet()` functions in `kdb.c` like the `gopts` plugin
* automatically mount the plugin in every backend
* generify the current plugin system so that global plugins can be defined like backend-specific plugins   

A decision has not yet been made. As the plugin is pretty self-contained, the choice should not effect the implementation of the plugin per se.

[QUESTION:] Is a new instance for a plugin loaded for every backend, or do the backends share a single instance of a plugin?

### Tooling

The tooling will be implemented as part of the `kdb` command line utility.

The most complex part of it will be the `kdb record export` command. 
When invoked, it must compute the minimal changeset of the session.
After that, it has to export the result into the specified format.
Parts of the existing `kdb export` functionality can probably be reused. 
 
