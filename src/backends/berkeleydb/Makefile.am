
# $Id$

#
# Compile libelektra-berkeleydb only if libdb was found
#

AM_CFLAGS=-g

if HAVE_DB
lib_LTLIBRARIES = libelektra-berkeleydb.la
libelektra_berkeleydb_la_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_berkeleydb_la_LDFLAGS = -version-info 1:0:0 -module
libelektra_berkeleydb_la_LIBADD = -ldb
libelektra_berkeleydb_la_CFLAGS = -Wall
INCLUDES = -I../../include
endif
