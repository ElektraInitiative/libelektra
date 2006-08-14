# $Id$
#
# NOTE: on INCLUDES i added $(XML_CFLAGS) too because
# it seems previous version of libxml macro export
# path in XML_CFLAGS rather than XML_CPPFLAGS
# -- Yannick

#if HAVE_XML

AM_CPPFLAGS = -DDATADIR=\"$(datadir)\" \
 -I$(top_srcdir)/src/include $(XML_CPPFLAGS) $(XML_CFLAGS)

ELEKTRATOOLS_CURRENT = 1

ektratools_sources = kdbtools.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h

noinst_LIBRARIES = libelektratools.a
libelektratools_a_SOURCES = $(ektratools_sources)
libelektratools_a_CFLAGS = -DELEKTRA_STATIC

ulib_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = $(ektratools_sources)

libelektratools_la_LDFLAGS = -version-info $(ELEKTRATOOLS_CURRENT):0:0
libelektratools_la_LIBADD = ../libelektra/libelektra.la $(XML_LIBS)

install-exec-hook:
	test -z "$(hlvlbackenddir)" || $(mkdir_p) "$(DESTDIR)$(hlvlbackenddir)"
	cd $(DESTDIR)$(hlvlbackenddir) && \
	test -e libelektratools.so || \
	$(LN_S) $(ulibdir)/libelektratools.so.$(ELEKTRATOOLS_CURRENT) libelektratools.so
#endif
