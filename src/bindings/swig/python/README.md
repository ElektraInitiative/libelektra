- infos =
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status =
- infos/provides = swig
- infos/description =

# Python

Python bindings for Elektra.

## Dependencies

For Python3 on debian jessie you need:

```sh
-DPYTHON_EXECUTABLE:PATH=/usr/bin/python3.4
-DPYTHON_LIBRARY:FILEPATH=/usr/lib/python3.4/config-3.4m-x86_64-linux-gnu/libpython3.4.so
-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.4
```

debian wheezy needs:

```sh
-DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python3
-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.2
-DPYTHON_LIBRARY:FILEPATH=/usr/lib/libpython3.2mu.so
```

## Building

Note that cmake does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.
