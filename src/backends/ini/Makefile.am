# $Id$

#testdir = $(develdocdir)/src/backends/ini/
AM_CPPFLAGS = -I$(top_srcdir)/src/include -DDEBUG -D_POSIX_SOURCE -D_BSD_SOURCE

ini_sources = ini.c helpers.c parser.c ini.h ../../include/kdb.h ../../include/kdbbackend.h
noinst_LIBRARIES = libelektra-ini.a
libelektra_ini_a_SOURCES = $(ini_sources)
libelektra_ini_a_CFLAGS = -DELEKTRA_STATIC

backend_LTLIBRARIES = libelektra-ini.la
libelektra_ini_la_SOURCES = $(ini_sources)
libelektra_ini_la_LDFLAGS = -avoid-version -module
libelektra_ini_la_LIBADD = ../../libelektra/libelektra.la

#EXTRA_DIST = test.c

check_PROGRAMS = test

test_SOURCES = test.c
test_LDADD = ../../libelektra/libelektra.la libelektra-ini.a

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

