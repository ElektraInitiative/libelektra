- infos = Information about Python bindings below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status = maintained
- infos/provides = swig
- infos/description =

# Python

Python bindings for Elektra.

If you have the Python bindings already installed,
you get started by reading the [tutorial](/doc/tutorials/python-kdb.md).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `python3-elektra`.

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

The default `CMAKE_INSTALL_PREFIX` (`/usr/local`) usually is
not part of Python's default search path, see `python3 -c 'import sys; print(sys.path)'`.
So either make sure to:

1. compile with `CMAKE_INSTALL_PREFIX=/usr`
2. tell CMake your correct Python site package with e.g. `PYTHON_SITE_PACKAGES=/usr/lib64/python3.y/site-packages`
3. extend Python's default search path at runtime with e.g. `PYTHONPATH=/usr/local/lib/python3/dist-packages`

## Development

Note that `cmake` does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.

## Iteration

Use external iterators the following way:

```python
size = ksSize(keySet)
for cursor in range(size):
	key = ksAt(keySet, cursor)
	# ...
```
