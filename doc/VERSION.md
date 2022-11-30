# Version

The version of Elektra is handled with the kdb.h macros
`KDB_VERSION` which is a string and `KDB_VERSION_MAJOR`,
`KDB_VERSION_MINOR` and `KDB_VERSION_PATCH` which are
numbers for the publicly announced versions.

The version can also be retrieved at run-time from KDB:

```
system:/elektra/version/constants/KDB_VERSION
system:/elektra/version/constants/KDB_VERSION_MAJOR
system:/elektra/version/constants/KDB_VERSION_MINOR
system:/elektra/version/constants/KDB_VERSION_PATCH
```

All libraries and plugins from Elektra must be installed
in exactly the same major and minor version, so no library
or plugin-specific version information exists.

## Scope

The version applies to following parts of Elektra:

- the API for programs using Elektra. Its interface
  is declared in [src/include/kdb.h](/src/include/kdb.h.in).
  Both applications and plugins use this API.
- the high-level API as declared in
  [src/include/elektra.h](/src/include/elektra.h).
- the API to plugins as declared in
  [src/plugins/doc/doc.h](/src/plugins/doc/doc.h).
- the API to [libease](/src/include/kdbease.h)
- the API to [libmeta](/src/include/kdbmeta.h)
- the API to [libmerge](/src/include/kdbmerge.h)
- the API to [module loading](/src/include/kdbmodule.h)
- the API to [libnotification](/src/include/kdbnotification.h)
- the API to [libopts](/src/include/kdbopts.h)
- plugins that have `infos/status` of `compatible`.
- the CLI tool `kdb` with its command-line
  arguments, KDB access and its return values.

## Behavior

The behavior of Elektra is defined by (in that order):

1. [ABI](/tests/abi) test cases
2. Non-ABI test cases (including Shell Recorder)
3. The [API documentation](https://doc.libelektra.org/api/master/html/)
4. The [man pages](/doc/help)
5. Tutorials (excluding Shell Recorder)
6. Examples

Any inconsistency within these artifacts within each other
or with the implementation constitutes a bug.

## Compatibility

This section describes under which circumstances API
and ABI incompatibilities may occur. As Elektra developer
your mission is to avoid any unwanted incompatibilities.
The tool icheck which checks the interfaces mentioned
in the Scope may help you.

Elektra uses a stricter version of
[Semantic Versioning 2.0.0](https://semver.org/),
with the extensions explained here.

In `1.0.*` the API and ABI must be always forward-compatible
and backwards-compatible.
That means that Elektra's libraries and plugins may be
upgraded and downgraded without any effect on applications,
only bug or documentation fixes are allowed.

In minor or patch version updates the API and ABI must be always forward-compatible,
but not backwards-compatible.
That means that a program written and compiled against 1.0.0
compiles and links against 1.1.0. But because it is
not necessarily backwards-compatible a program written
for 1.1.0 may not link or compile against Elektra 1.0.0
(but it compiles if you use the compatible subset, maybe
by using #ifdefs).
Also here applications must continue to work as originally
intended.

Changes in Elektra's datastructure will increase (at least)
the minor version level. This is required, as the cache plugin
exposes the datastructure and must have the same version as the
core and other libraries.

When you add a new function you break ABI and API backward-
compatibility (but not forward). You are only allowed to
do so in a `1.*` change.

In the signature you are only allowed to add const to
any parameter. You are _not_ allowed to use subtypes to
the objects, in C means you are not allowed to call any
functions of an object which appear new. C does _not_
type check that, it's your responsibility.

What C also does not check are the pre and postconditions.
That means you are not allowed to demand more client code
(e.g. first accept a NULL pointer and in the next version
you crash on it) and you are not allowed to return
values that the previous version did not return. It is
a complex topic, so better don't underestimate it, but
generally said the methods should behave on the same data
the same way.

As we use [symbol versioning](dev/symbol-versioning.md), the SO_VERSION
of Elektra always remains the same, even if the major version changes.
That means, if we rename or remove a function in the `2.0` release,
applications that linked against Elektra `1.0` can still link against
Elektra `2.0` but they do not necessarily compile with Elektra `2.0`
anymore. Once the code is adapted to use `2.0`, it cannot link against
`1.0` anymore. Note that this feature is only available on platforms
that support symbol versioning. For other platforms, all applications
need to be recompiled for every major version.

In major upgrades, changes of behavior is possible even if it might
break some use cases of some applications. Furthermore, symbols
that already have been deprecated before, might be removed.
For example, in `1.0` there is a deprecated method `ksNext` (not
available in `kdb.h`). Thus in `2.0` it is subject to be removed.

References:
https://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html
https://packages.debian.org/de/sid/icheck

## Bindings

Bindings are in general tied to Elektra's version number except
for the patch level. So bindings can only add or change
functionality when Elektra's core does.
This is a quite severe restriction but makes the version
much easier to understand for users. It serves the goal
that Elektra does not prefer any programming languages,
instead people expect from Elektra version `x.y.*`
identical functionality.

The patch level can be chosen by bindings as wanted.
Using the binding `x.y.z` does not mean that
Elektra `x.y.z` will be used on the target system.
Please read the changelog of the binding maintainers to
know about which bug fixes are included.

The patch level might also be used to fix bugs within bindings.
This means that applications can only introspect the patch
level of Elektra by getting `system:/elektra/version/constants/KDB_VERSION_PATCH`
but not by static patch levels the binding might provide.
This should be no problem, as the patch level is supposed
to not change the behavior.
