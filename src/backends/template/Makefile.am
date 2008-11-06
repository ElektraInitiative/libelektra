#elektemplatedir = $(develdocdir)/backend-template/

#EXTRA_DIST = template.c README

AM_CPPFLAGS = -I$(top_srcdir)/src/include

template_sources = template.c template.h ../../include/kdb.h ../../include/kdbbackend.h

noinst_LIBRARIES = libelektra-template.a
libelektra_template_a_SOURCES = $(template_sources)
libelektra_template_a_CFLAGS = -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)


backend_LTLIBRARIES = libelektra-template.la
libelektra_template_la_SOURCES = $(template_sources)
libelektra_template_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module
libelektra_template_la_LIBADD = ../../libelektra/libelektra.la
libelektra_template_la_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la

MAINTAINERCLEANFILES = \
	Makefile.in
