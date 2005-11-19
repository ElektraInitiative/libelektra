# $Id$

elekexampledir = $(develdocdir)/examples
                                                                                                                             
EXTRA_DIST = backend.c application.c binary.c keyNewExample.c lookup.c example.c hierarchy.xml keyset.xml

elekexample_DATA = backend.c application.c binary.c keyNewExample.c lookup.c

elekexample_PROGRAMS = backend application binary keyNewExample lookup

backend_SOURCES = backend.c
backend_LDFLAGS = -L../src/libelektra
backend_LDADD = -lelektra

application_SOURCES = application.c
application_LDFLAGS = -L../src/libelektra
application_LDADD = -lelektra

binary_SOURCES = binary.c
binary_LDFLAGS = -L../src/libelektra
binary_LDADD = -lelektra

keyNewExample_SOURCES = keyNewExample.c
keyNewExample_LDFLAGS = -L../src/libelektra
keyNewExample_LDADD = -lelektra

lookup_SOURCES = lookup.c
lookup_LDFLAGS = -L../src/libelektra
lookup_LDADD = -lelektra

INCLUDES = -I../src/include

