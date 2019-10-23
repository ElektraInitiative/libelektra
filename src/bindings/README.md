# Bindings

[Elektra’s API](https://doc.libelektra.org/api/latest/html/) is written in C,
but many different bindings allow _applications_ in different programming languages
to use Elektra.

Note that a binding does not automatically allow you to implement _plugins_
in the respective programming languages, but you additionally need an
[Interpreter Plugin](/src/plugins/README.md). Nevertheless, bindings
can be immediately used in applications without plugins.

List of currently supported bindings (use `ALL;-EXPERIMENTAL;-DEPRECATED`):

- [cpp](cpp/) C++11 bindings (included per default)
- [glib](glib/) GLib bindings
- [intercept_env](intercept/env/) Intercepts calls to environment (e.g. getenv())
- [swig_lua](swig/lua/) Lua SWIG bindings
- [swig_python](swig/python/) Python 3 SWIG bindings
- [swig_python2](swig/python2/) Python 2 SWIG bindings
- [swig_ruby](swig/ruby/) Ruby bindings
- [jna](jna/) A full java binding using JNA
- [rust](rust/) Bindings for the low-level API in Rust

Experimental bindings (included in `EXPERIMENTAL`):

- [gsettings](gsettings/) GLib bindings (experimental)
- [intercept_fs](intercept/fs/) Intercepts file system calls to configuration files (experimental)
- [io_uv](io/uv/) I/O binding for uv (experimental)
- [io_ev](io/ev/) I/O binding for ev (experimental)
- [io_glib](io/glib/) I/O binding for glib (experimental)

# I/O Bindings

These bindings allow Elektra to integrate into different main loop APIs using a
thin abstraction layer called "I/O binding".
The build all available I/O bindings use `-DBINDINGS="IO"` when configuring `cmake`.

For more information please check out the
[notification tutorial](https://github.com/ElektraInitiative/libelektra/tree/master/doc/tutorials/notifications.md)
or the
[API documentation](https://doc.libelektra.org/api/current/html/group__kdbio.html).
The [doc](io/doc/) directory contains an example binding for a fictive
asynchronous I/O management library.

## See Also

- See [COMPILE](/doc/COMPILE.md#bindings) for how to specify the bindings to build, e.g. `ALL`.
