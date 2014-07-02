To use python-legacy on debian you need following cmake defines:

        -DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python2.7
        -DPYTHON_INCLUDE_DIR:PATH=/usr/include/python2.7
        -DPYTHON_LIBRARY:FILEPATH=/usr/lib/libpython2.7.so

and in the code you need to import the new print

        from __future__ import print_function
        import kdb
