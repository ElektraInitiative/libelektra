# Bindings

[Elektra’s API](https://doc.libelektra.org/api/latest/html/) is written in C,
but many different bindings allow *applications* in different programming languages
to use Elektra.

Note that a binding does not automatically allow you to implement *plugins*
in the respective programming languages, but you additionally need an
[Interpreter Plugin](/src/plugins/README.md). Nevertheless, bindings
can be immediately used in applications without plugins.

List of currently supported bindings (included in `STABLE`):

- [cpp](cpp/) C++11 bindings (included per default)
- [jna](jna/) A full java binding using JNA
- [glib](glib/) GLib bindings
- [swig_lua](swig/lua/) Lua SWIG bindings
- [swig_python](swig/python/) Python 3 SWIG bindings
- [swig_python2](swig/python2/) Python 2 SWIG bindings
- [swig_ruby](swig/ruby/) Ruby bindings
- [intercept_env](intercept/env/) Intercepts calls to environment (e.g. getenv())
- [intercept_fs](intercept/fs/) Intercepts file system calls to configuration files (experimental)

Experimental bindings (included in `EXPERIMENTAL`):

- [gsettings](gsettings/) GLib bindings (experimental)
- [haskell](haskell/) Haskell bindings
- [io_uv](io/uv/) I/O bindings for uv

Deprecated bindings (included in `DEPRECATED`):

- [gi_python](gi/python/) GObject Introspection binding with Python specific overrides (deprecated)
- [gi_lua](gi/lua/) GObject Introspection binding with Lua specific overrides (deprecated)

## SEE ALSO

- See [COMPILE](/doc/COMPILE.md#bindings) for how to specify the bindings to build, e.g. `ALL`.
