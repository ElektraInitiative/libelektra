# Bindings #

[Elektra's API](http://doc.libelektra.org/api/latest/html/) is written in C,
but many different bindings allow *applications* in different programming languages
to use Elektra.

Note that a binding does not automatically allow you to implement *plugins*
in the respective programming languages, but you additionally need an
[Interpreter Plugins](/src/plugins/README.md). Nevertheless, bindings
can be immediately used in applications without plugins.

List of currently supported bindings:

- [cpp](cpp/) C++11 bindings
- [jna](jna/) A full java binding using JNA
- [glib](glib/) GLib bindings
- [gi_python](gi/python/) GObject Introspection binding with Python specific overrides
- [gi_lua](gi/lua/) GObject Introspection binding with Lua specific overrides
- [swig_lua](swig/lua/) Lua SWIG bindings
- [swig_python](swig/python/) Python 3 SWIG bindings
- [swig_python2](swig/python2/) Python 2 SWIG bindings (deprecated)
- [swig_ruby](swig/ruby/) Ruby bindings

See [COMPILE](/doc/COMPILE.md#bindings) how to specify the bindings to build.

