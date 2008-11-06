
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-fstab.a
libelektra_fstab_a_SOURCES = fstab.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_fstab_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

backend_LTLIBRARIES = libelektra-fstab.la
libelektra_fstab_la_SOURCES = fstab.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_fstab_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_fstab_la_LIBADD = ../../libelektra/libelektra.la
libelektra_fstab_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

clean-local:
	rm -f *.gcno *.gcda *.gcno

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

MAINTAINERCLEANFILES = \
	Makefile.in
