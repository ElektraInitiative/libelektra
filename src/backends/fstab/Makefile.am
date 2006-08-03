
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-fstab.a
libelektra_fstab_a_SOURCES = fstab.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_fstab_a_CFLAGS = -DELEKTRA_STATIC

backend_LTLIBRARIES = libelektra-fstab.la
libelektra_fstab_la_SOURCES = fstab.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_fstab_la_LDFLAGS = -avoid-version -module
libelektra_fstab_la_LIBADD = ../../libelektra/libelektra.la

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la
