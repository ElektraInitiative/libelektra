# $Id$

AM_CFLAGS = $(CSTDFLAGS) $(COPTFLAGS) $(CDBGFLAGS)
INCLUDES = -I$(top_srcdir)/src/include

EXTRA_DIST = small.c application.c binary.c keyNewExample.c lookup.c hellokdb.c hierarchy.xml keyset.xml iterate.c reference.c sort.c getset.c

# # Examples will only compile with make check
check_PROGRAMS = small application binary keyNewExample lookup cascading keyset hello functional hellokdb iterate reference sort getset

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

functional_SOURCES = functional.c
functional_LDADD = ../src/libelektra/libelektra.la

small_SOURCES = small.c
small_LDADD = ../src/libelektra/libelektra.la

hellokdb_SOURCES = hellokdb.c
hellokdb_LDADD = ../src/libelektra/libelektra.la

iterate_SOURCES = iterate.c
iterate_LDADD = ../src/libelektra/libelektra.la

reference_SOURCES = reference.c
reference_LDADD = ../src/libelektra/libelektra.la

sort_SOURCES = sort.c
sort_LDADD = ../src/libelektra/libelektra.la

getset_SOURCES = getset.c
getset_LDADD = ../src/libelektra/libelektra.la

clean-local:
	rm -f *.gcno *.gcda *.gcno
