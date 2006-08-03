
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include

lib_LIBRARIES = libelektra-filesys.a
libelektra_filesys_a_SOURCES = filesys.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_a_CFLAGS = -DELEKTRA_STATIC


backend_LTLIBRARIES = libelektra-filesys.la
libelektra_filesys_la_SOURCES = filesys.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_filesys_la_LDFLAGS = -avoid-version  -module
libelektra_filesys_la_LIBADD = ../../libelektra/libelektra.la

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la
