- infos =
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status =
- infos/provides = swig
- infos/description =

# Python

Python bindings for Elektra.

If you have the Python bindings already installed,
you get started by reading the [tutorial](/doc/tutorials/python-kdb.md).

## CMake Variables

To build the Python bindings
on Debian Stretch or Buster you need:

```sh
-DPython_ADDITIONAL_VERSIONS=`py3versions -d -v`
-DSWIG_EXECUTABLE=/usr/bin/swig3.0 \
-DPYTHON_EXECUTABLE:PATH=/usr/bin/python3 \
```

For Python3 on Debian Jessie you need:

```sh
-DPYTHON_EXECUTABLE:PATH=/usr/bin/python3.4
-DPYTHON_LIBRARY:FILEPATH=/usr/lib/python3.4/config-3.4m-x86_64-linux-gnu/libpython3.4.so
-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.4
```

On most distributions `CMAKE_INSTALL_PREFIX` needs to be set to `/usr`
so that Python finds the modules without setting `PYTHONPATH`.

With the default `CMAKE_INSTALL_PREFIX` (`/usr/local`) you need
to set `PYTHONPATH=/usr/local/lib/python3/dist-packages`.

## Development

Note that `cmake` does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.
