# $Id$
#
# NOTE: on INCLUDES i added $(XML_CFLAGS) too because
# it seems previous version of libxml macro export
# path in XML_CFLAGS rather than XML_CPPFLAGS
# -- Yannick

if HAVE_XML

ulib_LTLIBRARIES = libelektratools.la
libelektratools_la_SOURCES = kdbtools.c ../include/kdbtools.h ../include/kdbprivate.h ../include/kdb.h
libelektratools_la_CPPFLAGS = -DDATADIR=\"@datadir@\" -Wall \
  -I$(top_srcdir)/src/include $(XML_CPPFLAGS) $(XML_CFLAGS)
ELEKTRATOOLS_CURRENT = 1
libelektratools_la_LDFLAGS = -version-info $(ELEKTRATOOLS_CURRENT):0:0
libelektratools_la_LIBADD = ../libelektra/libelektra.la $(XML_LIBS)

install-exec-hook:
	test -z "$(hlvlbackenddir)" || $(mkdir_p) "$(DESTDIR)$(hlvlbackenddir)"
	cd $(DESTDIR)$(hlvlbackenddir) && \
	test -e libelektratools.so || \
	$(LN_S) $(ulibdir)/libelektratools.so.$(ELEKTRATOOLS_CURRENT) libelektratools.so
endif
