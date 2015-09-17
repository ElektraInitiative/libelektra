Please note that the python2 bindings has been deprecated!

To use python2 on debian you might need following cmake defines:

	-DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python2.7
	-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python2.7
	-DPYTHON_LIBRARY:FILEPATH=/usr/lib/libpython2.7.so

and in the code you need to import the new print

	from __future__ import print_function
	import kdb


For python3 on debian unstable you might need:

	-DPYTHON_LIBRARY:FILEPATH=/usr/lib/python3.4/config-3.4m-x86_64-linux-gnu/libpython3.4.so
	-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.4

debian stable needs:

	-DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python3
	-DPYTHON_INCLUDE_DIR:PATH=/usr/include/python3.2
	-DPYTHON_LIBRARY:FILEPATH=/usr/lib/libpython3.2mu.so 

Note that cmake does *not* automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.
