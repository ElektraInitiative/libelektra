
# $Id$

backend_LTLIBRARIES = libelektra-fstab.la
libelektra_fstab_la_SOURCES = fstab.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_fstab_la_LDFLAGS = -avoid-version -module
libelektra_fstab_la_CFLAGS = -Wall
AM_CPPFLAGS = -I$(top_srcdir)/src/include
libelektra_fstab_la_LIBADD = ../../libelektra/libelektra.la
