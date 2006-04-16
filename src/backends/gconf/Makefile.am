
# $Id$

#
# Compile libelektra-gconf only if GConf was found
#

if HAVE_GCONF
ulib_LTLIBRARIES = libelektra-gconf.la
libelektra_gconf_la_SOURCES = gconf.c 
libelektra_gconf_la_CFLAGS = $(gconf_CFLAGS) -Wall
libelektra_gconf_la_LDFLAGS = -avoid-version -module 
libelektra_gconf_la_LIBADD = $(LIBLTDL) $(gconf_LIBS)
INCLUDES = -I../../include
endif
