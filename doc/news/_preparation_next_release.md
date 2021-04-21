# 0.9.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial (checked by
shell recorder) or asciinema for highlighted items.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.9.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run
`docker run -it elektra/elektra`.

## Highlights

- Breaking change to `kdbOpen`. _[see below](#hl-1)_
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

<a id="hl-1"></a>

### `kdbOpen` Contracts

The signature of `kdbOpen` has been changed from

```c
KDB *  kdbOpen (Key * errorKey);
```

to

```c
KDB * kdbOpen(const KeySet * contract, Key *parentKey);
```

You can use `kdbOpen (NULL, errorKey)` to get the same behaviour as before.

The new parameter `contract` is similar to what could be done via `kdbEnsure` (which has been removed).
Currently, the contract allows you to mount global plugins and add data into the global KeySet (passed to all plugins)
during `kdbOpen`. This alone is already quite powerful, but we might more functionality in future releases.

For now, there are three use cases for the `contract` parameter. All of them are covered by helper functions:

```c
int elektraGOptsContract (KeySet * contract, int argc, const char * const * argv, const char * const * envp, const Key * parentKey, KeySet * goptsConfig);
int elektraIoContract (KeySet * contract, ElektraIoInterface * ioBinding);
int elektraNotificationContract (KeySet * contract);
```

With `elektraGOptsContract` you can mount and set up the `gopts` plugin used for command-line argument parsing.
The other two functions are the new way to configure Elektra's notification feature.

For more information take a look at [doc/dev/kdb-contracts.md](../dev/kdb-contracts.md)

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### Cache

- The `cache` plugin now only caches the parts of the global keyset that are below `system:/elektra/cache` or below
  `system:/elektra/cached`. The part below `system:/elektra/cache` is meant for internal data of the `cache`, so you
  should put data below `system:/elektra/cached`, if you want it to be cached. _(Klemens Böswirth)_
- <<TODO>>
- <<TODO>>

### Dbus

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### Dbusrecv

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### Zeromqsend

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### Zeromqrecv

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- `keyCopy` and `keyDup` now take an additional flag. See [below](#key-copy).
- `kdbEnsure` was removed and integrated into `kdbOpen`, which now takes an additional `KeySet * contract` parameter. See [above](#hl-1)
- <<TODO>>

### Core

<a id="key-copy"></a>

- The `keyCopy` and `keyDup` functions have been changed. They now take a `flags` argument which specifies which parts
  of the `Key` should be copied.
  The API also changed slightly. Most importantly `NULL` values are handled differently. For example, `keyDup (NULL, KEY_CP_ALL)`
  returns a key similar to what `keyNew ("/", KEY_END)` produces, whereas previously `keyDup (NULL)` returned `NULl`.
  _(Klemens Böswirth)_
- <<TODO>>
- We added `keyReplacePrefix`, a function that allows you to easily move a key from one parent to another. _(Klemens Böswirth)_
- `kdbEnsure` was removed and replaced by similar functionality added to `kdbOpen`. _[see above](#hl-1)_ _(Klemens Böswirth)_
- `KEY_END` is now defined as `(void *) 0` instead of `0`. This allows us to mark `keyNew` with the GCC attribute
  `__attribute__ ((sentinel))`, which causes a compiler warning, if `keyNew` calls don't use `KEY_END` as their last argument.
  _(Klemens Böswirth)_

### Io

- `elektraSetIoBinding` has been removed. Use `elektraIoContract` instead. _(Klemens Böswirth)_

### Notification

- `elektraNotificationOpen` has been removed. Use `elektraNotificationContract` instead.
  `elektraNotificationClose` has also been removed. There is no replacement, cleanup now happens automatially during
  `kdbClose`. _(Klemens Böswirth)_
- The contract for transport plugins has been changed. The exported functions `"openNotification"`, `"closeNotification" and`"setIoBinding"`are no longer used. Instead, plugins should retrieve the I/O binding from the key`system:/elektra/io/binding`in the global keyset. The notification callback and context that were passed to`"openNotification"`, can now be read from the global keyset as well. The keys are`system:/elektra/notification/callback`and`system:/elektra/notification/context` respectively.
  _(Klemens Böswirth)_

### <<Library3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### JNA

Since internal iterator support for `KeySet` is due to being dropped, the following methods have been removed:

- `Elektra::ksNext`
- `Elektra::ksCurrent`
- `Elektra::ksGetCursor`
- `Elektra::ksSetCursor`
- `KeySet::next`
- `KeySet::current`
- `KeySet::rewind`
- `KeySet::getCursor`
- `KeySet::setCursor`

Until internal `KeySet` iterator support has been dropped form native library, `Elektra::ksRewind` is being retained while also being annotated as 'depricated for removal'. The reason is, that we still need to rewind a `KeySet` before passing it to a native plugin via `NativePlugin::set`, `NativePlugin::get` or `NativePlugin::error`. _(@tucek)_

Further more `Elektra::ksPop` and `KeySet::pop` have been removed and `KeySet::remove` has been introduced as replacment.

_(@tucek)_

### <<Binding2>>

### <<Binding3>>

## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Make search for providers not skip rest of plugins on exceptions. _(Markus Raab)_

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Examples

- Fix enums in examples/spec. _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Reworked [METADATA.ini](/doc/METADATA.ini) _(Markus Raab)_
- Minor rewording in INSTALL.md _(@a-kraschitzer)_
- Write notes that `\\` are due to shell recorder, and are not to be copied _(Markus Raab)_
- Add link to [Go](https://github.com/ElektraInitiative/go-elektra) bindings _(Markus Raab)_
- Fix order of tutorials _(Markus Raab)_
- Added API-Reviews for multiple functions in the public API _(Stefan Hanreich)_
- Minor rewording in [java-kdb.md](/doc/tutorials/java-kdb.md) _(@aaronabebe)_
- Added a short Visual Studio 2019 tutorial (/doc/tutorials/contributing-windows.md) _(Dominic Jäger)_
- Added hint regarding WSL filesystem configuration (/doc/tutorials/contributing-windows.md) _(@tucek)_
- Fixed broken link in yanlr-plugin readme _(@lawli3t)_
- The example in the tutorial "mount-configuration-files" is adapted, so that it works now (fixed #3722) _(Philipp Oppel)_

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

- Change shlibs version compatibility policy of Debian packages to ">=". _(Robert Sowula)_
- Automate publishing of the release Elektra Docker images. _(Robert Sowula)_

## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- Update Alpine Linux images to version 3.13.1 and update Elektra release image. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Infrastructure

### Cirrus

- Update FreeBSD images from version 12.1 to 12.2 _(Robert Sowula)_
- <<TODO>>
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Jenkins

- Update daily job to always keep the latest Docker images containing installed Elektra packages that were build on master or during release. _(Robert Sowula)_
- Add a cleanup of the aptly database to the daily job. _(Robert Sowula)_
- <<TODO>>

### Travis

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- It is now possible to have two links on the same line of a markdown file rendered on the website. _(Klemens Böswirth)_
- The file [doc/KEYNAMES.md](../KEYNAMES.md) is now rendered on the website. _(Klemens Böswirth)_
- <<TODO>>

## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

<<`scripts/git-release-stats 0.9.VER-1 0.9.<<VERSION>>`>>

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums elektra-0.9.<<VERSION>>.tar.gz`>>

The release tarball is also available signed using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg?raw=true)

The following GPG Key was used to sign this release: 12CC44541E1B8AD9B66AFAD55262E7353324914A

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
