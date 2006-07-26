
# $Id$

#
# Compile libelektra-berkeleydb only if libdb was found
#

if HAVE_DB
backend_LTLIBRARIES = libelektra-berkeleydb.la
libelektra_berkeleydb_la_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_berkeleydb_la_LDFLAGS = -avoid-version  -module
libelektra_berkeleydb_la_LIBADD = -ldb
libelektra_berkeleydb_la_CFLAGS = -Wall
AM_CPPFLAGS = -I$(top_srcdir)/src/include
endif
