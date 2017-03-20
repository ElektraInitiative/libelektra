# Python2

Python2 bindings for Elektra.

## Dependencies

To use python2 on debian you might need following cmake defines:

	-DPYTHON2_EXECUTABLE:FILEPATH=/usr/bin/python2.7
	-DPYTHON2_INCLUDE_DIR:PATH=/usr/include/python2.7
	-DPYTHON2_LIBRARY:FILEPATH=/usr/lib/libpython2.7.so

and in the code you need to import the new print:

	from __future__ import print_function
	import kdb

## Infos

python2 is needed for the
[kdb gen tool](/src/tools/gen/gen)
because of its dependency to cheetah.

## Building

Note that cmake does *not* automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.

