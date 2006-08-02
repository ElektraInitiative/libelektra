
# $Id$

#
# Compile libelektra-berkeleydb only if libdb was found
#

if HAVE_DB
AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-berkeleydb.a
libelektra_berkeleydb_a_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_berkeleydb_a_CFLAGS = -Wall -DELEKTRA_STATIC

backend_LTLIBRARIES = libelektra-berkeleydb.la
libelektra_berkeleydb_la_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_berkeleydb_la_LDFLAGS = -avoid-version  -module
libelektra_berkeleydb_la_LIBADD = ../../libelektra/libelektra.la -ldb

../../libelektra/libelektra.la:
	cd ../../libelektra/ && make libelektra.la
endif
