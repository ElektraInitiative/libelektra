- infos =
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status =
- infos/provides = swig
- infos/description =

# Python

Python bindings for Elektra.

Get started by reading the [tutorial](/doc/tutorials/python-kdb.md).

## CMake Variables

To build the python binding
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

Debian Wheezy needs:

```sh
-DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python3
-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.2
-DPYTHON_LIBRARY:FILEPATH=/usr/lib/libpython3.2mu.so
```

## Development

Note that `cmake` does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.
