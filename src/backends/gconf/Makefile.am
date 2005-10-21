
# $Id$

#
# Compile libelektra-gconf only if GConf was found
#

if HAVE_GCONF
ulib_LTLIBRARIES = libelektra-gconf.la
libelektra_gconf_la_SOURCES = gconf.c 
libelektra_gconf_la_CFLAGS = $(gconf_CFLAGS)
libelektra_gconf_la_LDFLAGS = -version-info 1:0:0 -module 
libelektra_gconf_la_LIBADD = $(LIBLTDL) $(gconf_LIBS)
INCLUDES = -I../../include
endif
