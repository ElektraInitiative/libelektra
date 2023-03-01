- infos = Information about the specload plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/specload
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained nodep libc configurable unfinished
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

NOTE: currently the modifications a user can make are very limited/not possible. See [Limitations](#limitations) below.

## Dependencies

The plugin relies heavily on the `quickdump` plugin. It is used for storing the overridden values as well as the data transfer between
`specload` and an application.

To check whether `quickdump` is available you can use:

```
kdb plugin-list quickdump
#> quickdump
```

## Usage

To mount `specload` use (note: only the `spec` namespace is supported):

```
# specload will call '/usr/bin/exampleapp --elektra-spec'
kdb mount specload.eqd spec:/tests/specload/example specload 'app=/usr/bin/exampleapp'
```

You always have to specify the `app` that shall be called. This application will be called with the single argument `--elektra-spec`. This
can be configured by using `app/args/#` during mounting:

```
# specload will call '/usr/bin/exampleapp -o spec'
kdb mount specload.eqd spec:/tests/specload/example specload 'app=/usr/bin/exampleapp' 'app/args=#1' 'app/args/#0=-o' 'app/args/#1=spec'
```

To inhibit the default `--elektra-spec` argument and call an application without any arguments use `'app/args='`.

The app must only output the expected base specification to `stdout` and then exit, when called by `specload`. We use the `quickdump` plugin
for communication, so the application should call `elektraQuickdumpSet`. Because this dependency may change in future, it is recommended
you use the function `elektraSpecloadSendSpec` exported as `"system:/elektra/modules/specload/exports/sendspec"`:

```c
Key * errorKey = keyNew ("/", KEY_END);

// add 'system:/sendspec' key to suppress checking the 'app' key in elektraSpecloadOpen
KeySet * specloadConf = ksNew (1, keyNew ("system:/sendspec", KEY_END), KS_END);
ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, errorKey);

int result = elektraInvoke2Args (specload, "sendspec", ks, NULL);

elektraInvokeClose (specload, errorKey);
keyDel (errorKey);
ksDel (specloadConf);
```

The code above will automatically print the KeySet `ks` to stdout in exactly the way `specload` expects it.

Once the mounting is done you can inspect you specification like you would with any other mounted configuration. If you call `kdb set`
(or `kdb meta set` or anything else that calls `kdbSet()`), however, `specload` will verify that the modifications you made are compatible
with the original specification. Currently this verification is very restrictive and doesn't allow a lot of changes that would be safe.
This is because the necessary verification becomes very complex very quickly. For example adding `opt/arg` is only safe, if `opt` was also
added by the user, because the application might rely on the default `opt/arg=none`. See also [Limitations](#limitations).

### Direct File Mode

Instead of loading the specification via `stdin`/`stdout` from another app, you can also instruct `specload` to directly
load a `quickdump` file. This can be done by setting the `file` config key instead of `app`. The provided path must be
absolute, same as an `app` path.

Note: If `file` is specified, `app` will be ignored.

## Examples

This assumes you compiled the file [`testapp.c`](testapp.c) and it is available as the executable `testapp` in the current folder.

```
sudo kdb mount -R noresolver specload.eqd spec:/tests/specload specload "app=$(pwd)/testapp"

kdb meta-ls spec:/tests/specload/mykey
#> default

kdb get /tests/specload/mykey
#> 7

sudo kdb umount spec:/tests/specload

```

Or in direct file mode:

```sh
# This assumes that `$PWD` is the root of the Elektra source tree.
sudo kdb mount -R noresolver specload.eqd spec:/tests/specload specload "file=$(pwd)/src/plugins/specload/specload/spec.quickdump"

kdb meta-ls spec:/tests/specload/mykey
#> default

kdb get /tests/specload/mykey
#> 7

sudo kdb umount spec:/tests/specload
```

## Limitations

- The plugin only allows the following modifications right now:
  - add/edit/remove `description` or `opt/help`
  - add/edit `default`
  - add `type`
- The plugin must be mounted using `noresolver` or another resolver that always calls the storage plugin. To work around
  this limitation, `specload` prefixes relative paths with `KDB_DB_SPEC` before passing the path on to `quickdump`. This
  means that your overlay files will be stored in the same directory as any other `spec` configurations.
