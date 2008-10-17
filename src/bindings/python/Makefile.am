PYVER = 2.4

lib_LTLIBRARIES = libelektra.la
libelektra_la_SOURCES = libpyelektra.c
libelektra_la_LIBADD = ../../libelektra/libelektra.la -lpython$(PYVER)
libelektra_la_CFLAGS = -I../../include -I/usr/include/python$(PYVER)

EXTRA_DIST = examples application.py binary.py cascading.py elektra.py test.py
