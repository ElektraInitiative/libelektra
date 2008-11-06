AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-berkeleydb.a
libelektra_berkeleydb_a_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h
libelektra_berkeleydb_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

backend_LTLIBRARIES = libelektra-berkeleydb.la
libelektra_berkeleydb_la_SOURCES = berkeleydb.c ../../include/kdb.h ../../include/kdbbackend.h 
libelektra_berkeleydb_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_berkeleydb_la_LIBADD = ../../libelektra/libelektra.la -ldb
libelektra_berkeleydb_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

MAINTAINERCLEANFILES = \
	Makefile.in
