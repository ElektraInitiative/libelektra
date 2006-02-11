# $Id$

elekexampledir = $(develdocdir)/examples
                                                                                                                             
EXTRA_DIST = test.c application.c binary.c keyNewExample.c lookup.c example.c hierarchy.xml keyset.xml

elekexample_DATA = test.c application.c binary.c keyNewExample.c lookup.c

elekexample_PROGRAMS = test application binary keyNewExample lookup hello

test_SOURCES = test.c
test_LDFLAGS = -L../src/libelektra
test_LDADD = -lelektra

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

hello_SOURCES = hello.c
hello_LDFLAGS = -L../src/libelektra
hello_LDADD = -lelektra

INCLUDES = -I../src/include

