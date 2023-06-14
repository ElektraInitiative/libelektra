# Symbol Versioning

To enable backwards compatibility, while allowing the API to evolve, we use symbol versioning. This document focuses on the setup Elektra
uses. For an explanation of symbol versioning itself, you may start [here](https://people.freebsd.org/~deischen/symver/library_versioning.txt).

## Infrastructure

The setup used is based on the one used by FreeBSD. The main script [`version_gen.awk`](/src/libs/version_gen.awk) is taken from their source
(with small modifications to enable Alpine Linux compatibility).

The main script, is called by CMake during the build process. It parses the `symbols.map` files of all libraries (e.g.
[`/src/libs/core/symbols.map`](/src/libs/core/symbols.map)) and combines them into a single file, which is then used by the compiler
and the linker.

### `versions.def`

All versions of Elektra symbols have to be declared in [`versions.def`](/src/libs/versions.def). Only the versions declared in this file
can be used in the `symbols.map` files. No symbols should be declared in this file.

Each version corresponds to a release. After each release of Elektra, a new version must be added to `versions.def`. Only this version may
be modified in the `symbols.map` files. Released version MUST NOT change. The newest and therefore unreleased version may change at any point
before its release.

The `libelektraprivate_1.0` is special. It MUST always declare the newest version (e.g. `libelektra_0.9`) as its base and it is the only
private version (i.e. there is will be no `libelektraprivate_1.1`). The purpose of the private version is to allow the use of internal
symbols across compile units. For example `elektraAbort` is part of this version. It should be usable in all of Elektra's modules, but not
outside. Currently there is no mechanism to enforce this, however, developers can manually check, if the use any of the symbols declared in
`libelektraprivate_1.0`. Any of these symbols may change or removed at any time. No compatibility guarantee is given for these symbols.

## Exporting new symbols

To add a new symbol (probably a function), simply add its name to the `symbols.map` file of the relevant library. Make sure to always add
the symbol to the newest version only. Only symbols listed in one of the `symbols.map` files will be accessible by the linker. This means
only these symbols can be used outside of their compilation unit. Only export symbols, if this is your intention.

For public API this is an obvious decision. All public API function should be added to newest version that exists at the time of their
creation.

For internal API the question is not so obvious. If possible use static symbols. Static symbols cannot be exported via `symbols.map`, so the
question is trivial. If the symbol shall be used in different source files (and therefore cannot be static), but only inside a single
compilation unit (°), you don't need to add the symbol. If you are unsure, first try it without adding the symbol to `symbols.map` and only
if it doesn't work add the symbol to `libelektraprivate_1.0`.

(°) compilation units mostly correspond to libraries

## Updating a symbol

If a symbol is part of the public API (i.e. not in `libelektraprivate_1.0`) and you want to make changes that are ABI incompatible (e.g.
changing the signature of a function), you have to update the symbol version.

To do this, simply add the symbol name to the newest version in `symbols.map` and remove it from the version it is currently part of. Each
symbol may only be defined in one version. If the symbol is already declared in the latest version, you don't need to do anything, unreleased
version may be modified at any point in time.

For additional clarity, you may leave a comment in the old version noting where the symbols was moved to.

To actually implement symbol versioning, a few code changes are necessary. Explaining this is much easier, when using a concrete example.

We will use the function `int foo (Key * k)` and assume the new version will have the signature `int foo (Key * k, int x)`. The old version
shall be called `oldversion` and the new version shall be `newversion`.

The old function declaration

```c
int foo (Key * k);
```

has to be replaced with the new declarations

```c
int foo (Key * k, int x);

int ELEKTRA_SYMVER (foo, v1) (Key * k);
```

The second line declares an old version of `foo`.
Note: `v1` may be replaced by any string that is a valid identifier. It is simply their to make the symbol name unique.

In the source files containing the definition of our function, we change the old definition

```c
int foo (Key * k)
{
    // old code ...
}
```

to use the compatibility symbol

```c
int ELEKTRA_SYMVER (foo, v1) (Key * k)
{
    // old code ...
}

ELEKTRA_SYMVER_DECLARE ("oldversion", foo, v1)
```

To actually enable backwards compatibility, we need to add some assembler `.symver` pseudo instructions. To make this easier, we have the
helper macro `ELEKTRA_SYMVER_DECLARE`. We add these lines after the function declaration.
Note: there MUST NOT be a `;` after the `ELEKTRA_SYMVER_DECLARE` otherwise there will be errors on systems that don't support symbol
versioning

Then we simply write the definition for the new version, as if it was an entirely new function:

```c
int foo (Key * k, int x)
{
    // new code ...
}
```

The current version of a symbol always uses the unversioned name of the symbol. That way we default to this version. Only older versions use
a versioned name via `ELEKTRA_SYMVER`.

## Removing a symbol

If a symbol becomes deprecated, it should NOT be removed from the `symbols.map` file. This would break backwards compatibility. Instead
we remove the default implementation and only leave versions implemented via `ELEKTRA_SYMVER` and `ELEKTRA_SYMVER_DECLARE`. The existing
default version must be converted into a versioned symbol for the last supported version.
