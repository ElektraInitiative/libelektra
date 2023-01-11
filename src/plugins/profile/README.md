- infos = Information about the profile plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = nodep hook experimental
- infos/metadata =
- infos/description = helps switching between configuration profiles

## Usage

Following the elektra keyname convention application configurations are stored under `/sw/org/myapp/#` and `/sw/org/myapp/#0/current`is the profile to be used.
The `profile` plugin provides an easy way to switch configuration profiles.

The key `/sw/org/myapp/#0/profile` defines what profile should be used as `current`, e.g. `/sw/org/myapp/#0/profile = myprofile`.
If a key `/sw/org/myapp/#0/myprofile/key` is found and no key `/sw/org/myapp/#0/current/key` exists an override key will be created linking `/sw/org/myapp/#0/currrent/key` to `/sw/org/myapp/#0/myapp/key`
If neither `/sw/org/myapp/#0/current/key` nor `/sw/org/myapp/#0/myprofile/key` is found, but `/sw/org/myapp/#0/%/key`, `/sw/org/myapp/#0/current/key` will be linked to `/sw/org/myapp/#0/%/key`.

So a cascading lookup will automatically implement following
preferences (next to the namespace preferences):

1. Usage of key in `current`
2. Usage of key in the profile set with `profile`
3. Usage of key in the `%` fallback profile

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Example

<!-- FIXME [new_backend]: outdated -->

Suppose we have the configuration file `profile.ini` in `~/.config`:

```sh
cat profile.ini
#> []
#> profile = myprofile
#>
#> [current]
#> key2 = will win
#>
#> [myprofile]
#> key1 = test1
#> key2 = test2
#>
#> [%]
#> key2 = failed?
#> key3 = test3
```

Then we simply mount it _without_ the profile plugin:

```sh
kdb mount profile.ini /sw/org/myapp/#0 ini
```

But we have to make sure that the profile plugin is mounted globally:

```sh
kdb global-mount profile
```

Then we can access `/sw/org/myapp/#0` in a profile-aware way:

```sh
kdb ls /sw
#> spec:/sw/org/myapp/#0/current/key1
#> spec:/sw/org/myapp/#0/current/key3
#> user:/sw/org/myapp/#0
#> user:/sw/org/myapp/#0/%
#> user:/sw/org/myapp/#0/%/key2
#> user:/sw/org/myapp/#0/%/key3
#> user:/sw/org/myapp/#0/current
#> user:/sw/org/myapp/#0/current/key2
#> user:/sw/org/myapp/#0/profile
#> user:/sw/org/myapp/#0/myprofile
#> user:/sw/org/myapp/#0/myprofile/key1
#> user:/sw/org/myapp/#0/myprofile/key2
```

As we can see with the `-v` option, we will fetch keys from our `myprofile` even though we request `current`:

```sh
kdb get -v /sw/org/myapp/#0/current/key1
#> got 25 keys
#> searching spec:/sw/org/myapp/#0/current/key1, found: spec:/sw/org/myapp/#0/current/key1, options: KDB_O_CALLBACK
#> The resulting keyname is user:/sw/org/myapp/#0/myprofile/key1
#> test1
```

To switch profile we simply have to set one key:

```sh
kdb set user:/sw/org/myapp/#0/profile newprofile
```

Usually, this will be done via commandline by setting `proc:/sw/org/myapp/#0/profile`.
