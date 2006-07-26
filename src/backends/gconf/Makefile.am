
# $Id$

#
# Compile libelektra-gconf only if GConf was found
#

if HAVE_GCONF
hlvlbackend_LTLIBRARIES = libelektra-gconf.la
libelektra_gconf_la_SOURCES = gconf.c 
libelektra_gconf_la_CFLAGS = $(gconf_CFLAGS) -Wall
libelektra_gconf_la_LDFLAGS = -avoid-version -module 
libelektra_gconf_la_LIBADD = $(LIBLTDL) $(gconf_LIBS) ../../libelektra/libelektra.la
AM_CPPFLAGS = -I$(top_srcdir)/src/include
endif
