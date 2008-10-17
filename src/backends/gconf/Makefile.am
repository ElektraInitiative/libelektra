
# $Id$

#
# Compile libelektra-gconf only if GConf was found
#

#if HAVE_GCONF
AM_CPPFLAGS = -I$(top_srcdir)/src/include

noinst_LIBRARIES = libelektra-gconf.a
libelektra_gconf_a_SOURCES = gconf.c
libelektra_gconf_a_CFLAGS = $(gconf_CFLAGS) -DELEKTRA_STATIC $(COPTFLAGS) $(CDBGFLAGS)

hlvlbackend_LTLIBRARIES = libelektra-gconf.la
libelektra_gconf_la_SOURCES = gconf.c 
libelektra_gconf_la_CFLAGS = $(gconf_CFLAGS) $(COPTFLAGS) $(CDBGFLAGS)
libelektra_gconf_la_LDFLAGS = -version-info $(BACKEND_VERSION_API) -module 
libelektra_gconf_la_LIBADD = $(gconf_LIBS) ../../libelektra/libelektra.la

../../libelektra/libelektra.la:
	cd ../../libelektra/ && $(MAKE) libelektra.la
#endif
