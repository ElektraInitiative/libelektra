# $Id$

elekexampledir = $(develdocdir)/examples
                                                                                                                             
EXTRA_DIST = test.c application.c binary.c keyNewExample.c lookup.c example.c hierarchy.xml keyset.xml

elekexample_DATA = test.c application.c binary.c keyNewExample.c lookup.c

#
# Examples will only compile if you enable experimental
# you can also remove the if EXPERIMENAL and endif and `make` again
#

if EXPERIMENTAL
elekexample_PROGRAMS = test application binary keyNewExample lookup cascading keyset hello

test_SOURCES = test.c
test_LDADD = ../src/libelektra/libelektra.la

application_SOURCES = application.c
application_LDADD = ../src/libelektra/libelektra.la

binary_SOURCES = binary.c
binary_LDADD = ../src/libelektra/libelektra.la

keyNewExample_SOURCES = keyNewExample.c
keyNewExample_LDADD = ../src/libelektra/libelektra.la

lookup_SOURCES = lookup.c
lookup_LDADD = ../src/libelektra/libelektra.la

cascading_SOURCES = cascading.c
cascading_LDADD = ../src/libelektra/libelektra.la

keyset_SOURCES = keyset.c
keyset_LDADD = ../src/libelektra/libelektra.la

hello_SOURCES = hello.c
hello_LDADD = ../src/libelektra/libelektra.la
endif


AM_CPPFLAGS = -I$(top_srcdir)/src/include

