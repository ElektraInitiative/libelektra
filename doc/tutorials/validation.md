# Validation

## Introduction

Configuration in <abbr title="Free/Libre and Open Source Software">FLOSS</abbr> unfortunately is often stored
completely without validation. Notable exceptions are sudo
(`visudo`), or user accounts (`adduser`) but in most cases
you only get feedback of non-validating configuration when
the application fails to start.

Elektra provides a generic way to validate any configuration
before it is written to disc.

## User Interfaces

Any of Elektra's user interfaces will work with the technique
described in this tutorial, e.g.:

1. `kdb qt-gui`: graphical user interface

2. `kdb editor`: starts up your favorite text editor and
   allows you to edit configuration in any syntax.
   (generalization of `visudo`)

3. `kdb set`: manipulate or add individual configuration
   entries.
   (generalization of `adduser`)

4. Any other tool using Elektra to store configuration
   (e.g. if the application itself has capabilities to
   modify its configuration)

## Metadata Together With Keys

The most direct way to validate keys is

```sh
kdb meta set user:/tests/together/test check/validation "[1-9][0-9]*"
kdb meta set user:/tests/together/test check/validation/match LINE
kdb meta set user:/tests/together/test check/validation/message "Not a number"
kdb set user:/tests/together/test 123
#> Set string to "123"

# Undo modifications
kdb rm -r user:/tests/together
```

The approach is not limited to validation via regular expressions, but
any values-validation plugin can be used, e.g. [type](/src/plugins/type).
For a full list refer to the section "Value Validation" in the
[list of all plugins](/src/plugins/README.md).

Note that it's also easy [to write your own (value validation) plugin](/doc/tutorials/plugins.md).

The drawbacks of this approach are:

- The administrator needs to take care that the validation plugins are available
  where needed.
- Some metadata (in this case `check/validation`) needs to be stored next to
  the key which won't work with most configuration files.
  This is the reason why we explicitly used `dump` as storage in `kdb mount`.
- After the key is removed, the validation information is gone, too.
- You cannot validate structure of which keys must be present or absent.

## Get Started with `spec`

These issues are resolved straightforward by separating the configuration from
its configuration specification (often called schemata in XML or JSON).
The purpose of the [spec namespace](/doc/tutorials/namespaces.md) is to hold the
configuration specification, i.e., the description of how to validate the keys of
all other namespaces.

To make this work, we need a plugin that applies all metadata found in the `spec`-namespace
to all other namespaces. This plugin is called `spec` and needs to be mounted
globally (will be added by default and also with any `kdb global-mount` call).

Before we start, let us make a backup of the current data in the spec and user namespace:

```sh
kdb set system:/tests/specbackup $(mktemp)
kdb set system:/tests/userbackup $(mktemp)
kdb export spec:/ dump > $(kdb get system:/tests/specbackup)
kdb export user:/ dump > $(kdb get system:/tests/userbackup)
```

We write metadata to the namespace `spec` and the plugin `spec` applies it to every cascading key:

```sh
kdb meta set spec:/tests/spec/test hello world
kdb set user:/tests/spec/test value
kdb meta ls spec:/tests/spec/test | grep -v '^internal/ini'
#> hello
kdb meta ls /tests/spec/test | grep -v '^internal/ini'
#> hello
kdb meta get /tests/spec/test hello
#> world
```

But it also supports globbing (`_` for any key, `?` for any char, `[]` for character classes):

```sh
kdb meta set "spec:/tests/spec/_" new metaval
kdb set user:/tests/spec/test value
kdb meta ls /tests/spec/test | grep -v '^internal/ini'
#> hello
#> new

# Remove keys and metadata from the commands above
kdb rm -r spec:/tests/spec
kdb rm -r user:/tests/spec || kdb rm -r system:/tests/spec
```

So let us combine this functionality with validation plugins.
So we would specify:

```sh
kdb meta set spec:/tests/spec/test check/validation "[1-9][0-9]*"
kdb meta set spec:/tests/spec/test check/validation/match LINE
kdb meta set spec:/tests/spec/test check/validation/message "Not a number"
```

If we now set a new key with

```sh
kdb set user:/tests/spec/test "not a number"
#> Create a new key user:/tests/spec/test with string "not a number"
```

this key has adopted all metadata from the spec namespace:

```sh
kdb meta ls /tests/spec/test | grep -v '^internal/ini'
#> check/validation
#> check/validation/match
#> check/validation/message
```

Note that this key should not have passed the validation that we defined in the
spec namespace.
Nonetheless we were able to set this key, because the validation plugin was not
active for this key.
On that behalf we have to make sure that the validation plugin is loaded for
this key with:

```sh
kdb mount tutorial.dump /tests/spec dump validation
```

This [mounts](/doc/tutorials/mount.md) the backend `tutorial.dump` to the mount point
**/tests/spec** and activates the validation plugin for the keys below the mount point.
The validation plugin now uses the metadata of the keys below **/tests/spec**
to validate values before storing them in `tutorial.dump`.

If we try setting the key again, we will get an error:

```sh
kdb set user:/tests/spec/test "not a number"
# STDERR: .*Validation Syntactic.*Not a number.*
# ERROR:  C03100
# RET: 5
```

However, if we add a key that adheres to the validation rules, it will work:

```sh
kdb set user:/tests/spec/test 42
#> Create a new key user:/tests/spec/test with string "42"
```

Although this is better than defining metadata in the same place as the data
itself, we can still do better.
The reason for that is that one of the aims of Elektra is to remove the trouble
of validation and finding the files that hold your configuration from the users.
At the moment a user still has to know which files should hold the configuration
and which plugins must be loaded when they mount configuration files.

This problem can be addressed by recognizing that the location of the
configuration files and the plugins that must be loaded is part of the
_schema_ of our configuration and therefore should be stored in the spec namespace.

```sh
# Undo modifications
kdb rm -r spec:/tests/spec
kdb rm -r user:/tests/spec || kdb rm -r system:/tests/spec
kdb umount /tests/spec
```

### Specfiles

We call the files, that contain a complete schema for configuration
below a specific path in form of metadata, _Specfiles_.

A _Specfile_ contains metadata, among others, that defines how
the configuration settings should be validated.

Let us create an example _Specfile_ in the dump format, which supports metadata.
Although the specfile is stored in the dump format, we can still create it using
the human-readable [ni format](/src/plugins/ni/README.md) by using `kdb import`
(note that the `\\` are due to [Markdown Shell Recorder][], do not copy them to your shell):
[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

```sh
sudo kdb mount tutorial.dump spec:/tests/tutorial dump
cat << HERE | kdb import spec:/tests/tutorial ni  \
[]                                         \
 mountpoint=tutorial.dump                \
 infos/plugins=dump validation           \
                                           \
[/links/_]                                 \
check/validation=https?://.*\..*         \
check/validation/match=LINE              \
check/validation/message=not a valid URL \
description=A link to some website       \
HERE
kdb meta ls spec:/tests/tutorial
#> infos/plugins
#> mountpoint
```

We now have all the metadata that we need to mount and validate the data below
`/tutorial` in one file.

For a description which metadata is available, have a look in
[METADATA.ini](/doc/METADATA.ini).

Now we apply this _Specfile_ to the key database to all keys below `tests/tutorial`.

```sh
kdb spec-mount /tests/tutorial
```

This command automatically mounts `/tests/tutorial` to the backend `tutorial.dump`.
Furthermore it adds all plugins necessary for all metadata within the specification.
So in this example the validation plugin will be loaded automatically for us.
`spec-mount` basically does a normal mount except that it automatically selects plugins. As a result there is no
`spec-umount` command since the normal `umount` is sufficient.

Please be aware that if you require many plugins for the same mount point,
you can run into [this](https://github.com/ElektraInitiative/libelektra/issues/2133) error.

```sh
kdb set user:/tests/tutorial/links/url "invalid url"
# STDERR: .*Validation Syntactic.*not a valid URL.*
# ERROR:  C03100
# RET:    5
```

Note that the backend `tutorial.dump` is mounted for all namespaces:

```sh
kdb file user:/tests/tutorial
# STDOUT-REGEX: /.*/tutorial\.dump
kdb file system:/tests/tutorial
# STDOUT-REGEX: /.*/tutorial\.dump
kdb file dir:/tests/tutorial
# STDOUT-REGEX: /.*/tutorial\.dump
```

If you want to go without validation, you can work around that by setting the keys with the `-f` (`--force`) option:

```sh
kdb set -f system:/tests/tutorial/links/elektra "invalid url"
#> Create a new key system:/tests/tutorial/links/elektra with string "invalid url"
```

## Rejecting Configuration Keys

Up to now we only discussed how to reject keys that have unwanted values.
Sometimes, however, applications require the presence or absence of keys.
There are many ways to do so directly supported by [the spec plugin](/src/plugins/spec).
Another way is to trigger errors with the [error plugin](/src/plugins/error):

```sh
kdb meta set spec:/tests/tutorial/spec/should_not_be_here trigger/error C03200
kdb spec-mount /tests/tutorial
kdb set user:/tests/tutorial/spec/should_not_be_here abc
# RET:    5
# ERROR:C03200
kdb get /tests/tutorial/spec/should_not_be_here
# RET: 11
# STDERR: Did not find key '/tests/tutorial/spec/should_not_be_here'
```

If we want to reject every optional key (and only want to allow required keys)
we can use the plugin `required` as further discussed below.

Before we look further let us undo the modifications to the key database.

```sh
kdb rm -r spec:/tests/tutorial
kdb rm -r system:/tests/tutorial
kdb rm -rf user:/tests/tutorial
kdb umount spec:/tests/tutorial
kdb umount /tests/tutorial
kdb rm -rf spec:/
kdb rm -rf user:/
kdb import spec:/ dump < $(kdb get system:/tests/specbackup)
kdb import user:/ dump < $(kdb get system:/tests/userbackup)
rm $(kdb get system:/tests/specbackup)
rm $(kdb get system:/tests/userbackup)
kdb rm system:/tests/specbackup
kdb rm system:/tests/userbackup
```

## Validate Existing Keys

To check if an existing set of keys can be read and written with the current validation rules `kdb validate` should be used. Validate will read the values of all string keys under the point defined as argument in the command line, sets the key value to something different, then back to the original and finally writes that original value back to the key database. All loaded [validation plugins](/src/plugins/README.md) are now used to validate the values of keys with the necessary meta-keys (see above).

Only string keys are validated! Binary keys are skipped!

```sh
# mount test config file and set a value
sudo kdb mount range.ecf /tests/range range dump

# set value
kdb set user:/tests/range/value 5

# add range check to all keys under /tests/range/
kdb meta set spec:/tests/range/_ check/range "1-10"

# check if validate passes
kdb validate /tests/range

# set new key to invalid value (with kdb set -f)
kdb set -f user:/tests/range/value2 11

# validation fails now
kdb validate /tests/range
# RET:11

# clean up
kdb rm -r /tests/range/
sudo kdb umount /tests/range

```
