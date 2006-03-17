# $Id$

elekexampledir = $(develdocdir)/examples
                                                                                                                             
EXTRA_DIST = test.c application.c binary.c keyNewExample.c lookup.c example.c hierarchy.xml keyset.xml

elekexample_DATA = test.c application.c binary.c keyNewExample.c lookup.c

#
# Examples will only compile if you enable experimental
# you can also remove the if EXPERIMENAL and endif and `make` again
#

if EXPERIMENTAL
elekexample_PROGRAMS = test application binary keyNewExample lookup cascading

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

cascading_SOURCES = cascading.c
cascading_LDFLAGS = -L../src/libelektra
cascading_LDADD = -lelektra
endif


INCLUDES = -I../src/include

