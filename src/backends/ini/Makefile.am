# $Id$

#testdir = $(develdocdir)/src/backends/ini/
AM_CPPFLAGS = -I$(top_srcdir)/src/include -D_POSIX_SOURCE -D_BSD_SOURCE

ini_sources = ini.c helpers.c parser.c ini.h ../../include/kdb.h ../../include/kdbbackend.h
noinst_LIBRARIES = libelektra-ini.a
libelektra_ini_a_SOURCES = $(ini_sources)
libelektra_ini_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

backend_LTLIBRARIES = libelektra-ini.la
libelektra_ini_la_SOURCES = $(ini_sources)
libelektra_ini_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_ini_la_LIBADD = ../../libelektra/libelektra.la
libelektra_ini_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

MAINTAINERCLEANFILES = \
	Makefile.in
