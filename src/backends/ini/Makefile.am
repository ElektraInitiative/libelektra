# $Id$

testdir = $(develdocdir)/src/backends/ini/

lib_LIBRARIES = libelektra-ini.a
libelektra_ini_a_SOURCES = ini.c helpers.c parser.c ini.h ../../include/kdb.h ../../include/kdbbackend.h
libelektra_ini_a_CFLAGS = -DDEBUG -D_POSIX_SOURCE -D_BSD_SOURCE -D__STATIC -ansi -pedantic -Wall
libelektra_ini_a_LIBADD = ../../libelektra/libelektra.la

backend_LTLIBRARIES = libelektra-ini.la
libelektra_ini_la_SOURCES = ini.c helpers.c parser.c ini.h ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_ini_la_LDFLAGS = -avoid-version -module
libelektra_ini_la_CFLAGS = -DDEBUG -D_POSIX_SOURCE -D_BSD_SOURCE -ansi -pedantic -Wall
libelektra_ini_la_LIBADD = ../../libelektra/libelektra.la
AM_CPPFLAGS = -I$(top_srcdir)/src/include

#EXTRA_DIST = test.c

check_PROGRAMS = test

test_SOURCES = test.c
test_LDADD = ../../libelektra/libelektra.la libelektra-ini.a

