# Svn Stuff
# $Id$
# $LastChangedBy: rayman $

#Version of package
VERSION = @VERSION@

#library interface
INTERFACE = 2

slibdir = @slibdir@
debug_parms = @debug_parms@
#not sure what the hell I need CPP flags for since there is no cpp needed?
#CPPFLAGS = @CPPFLAGS@ -D_REENTRANT

INCLUDES = @ELEKTRA_CFLAGS@
AM_CFLAGS = @CFLAGS@ -W -Wall -Wstrict-prototypes -Wpointer-arith -Wundef $(debug_parms)

slib_LTLIBRARIES = libnss_elektra.la

libnss_elektra_la_SOURCES = lib.c passwd.c shadow.c group.c

noinst_HEADERS = lib.h passwd.h nss-elektra.h config.h nss-shadow.h group.h

libnss_elektra_la_LDFLAGS = -module -version-info $(INTERFACE)
libnss_elektra_la_LIBADD = @ELEKTRA_LIBS@ 
