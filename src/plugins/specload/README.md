- infos = Information about the specload plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs = quickdump
- infos/provides =
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = nodep libc configurable experimental unfinished
- infos/metadata =
- infos/description = loads a specification from an external application

## Introduction

In order to optimally use Elektra an application has to provide a specification. This specification has to be mounted in the `spec`
namespace by the user (or automatically during the install process). However, this specification should in most cases not be modified by
the user. If it is modified the user has to be sure it is still compatible to the original specification. Otherwise the application might
crash because it e.g. expected a default value to be provided by the specification that the user changed or removed.

This is were this plugin comes in. Unlike other storage plugins `specload` doesn't use the file given during mounting for most of the
configuration. Instead `specload` requests the specification with which an application was compiled from the application itself. On top
of this base specifications the user can make some modifications, which are the only keys stored in the mounted file. All modifications
made by the user are verified by the plugin. The user can never modify the specification in a way to would break application compatibility.

NOTE: currently the modifications a user can make are very limited. See [Limitations](#limitations) below.

## Usage

To use `specload` first make sure Elektra was compiled with `BUILD_SHARED` enabled, and the `quickdump` plugin is available as well:

```
kdb list | grep '^quickdump$'
#> quickdump
```

The next step is to mount `specload` (note: only the `spec` namespace is supported):

```
# specload will call '/usr/bin/exampleapp --elektra-spec'
kdb mount specload.eqd spec/tests/specload/example specload 'app=/usr/bin/exampleapp'
```

You always have to specify the `app` that shall be called. This application will be called with the single argument `--elektra-spec`. This
can be configured by using `app/args/#` during mounting:

```
# specload will call '/usr/bin/exampleapp -o spec'
kdb mount specload.eqd spec/tests/specload/example specload 'app=/usr/bin/exampleapp' 'app/args=#1' 'app/args/#0=-o' 'app/args/#1=spec'
```

To inhibit the default `--elektra-spec` argument and call an application without any arguments use `'app/args='`.

Once the mounting is done you can inspect you specification like you would with any other mounted configuration. If you call `kdb set`
(or `kdb setmeta` or anything else that calls `kdbSet()`), however, `specload` will verify that the modifications you made are compatible
with the original specification. Currently this verification is very restrictive and doesn't allow a lot of changes that would be safe.
This is because the necessary verification becomes very complex very quickly. For example adding `opt/arg` is only safe, if `opt` was also
added by the user, because the application might rely on the default `opt/arg=none`. See also [Limitations](#limitations).

## Dependencies

The plugin relies heavily on the `quickdump` plugin. It is used for storing the overridden values as well as the data transfer between
`specload` and an application.

## Examples

TODO

## Limitations

- Currently the plugin is only supported in SHARED builds.
- For now only modifying metadata in one of these ways is allowed:
  - add/edit/remove `description` or `opt/help`
  - add/edit `default`
  - add `type`
