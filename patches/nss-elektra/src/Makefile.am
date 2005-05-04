# Svn Stuff
# $Id: Makefile.am 40 2004-11-26 23:25:09Z rayman $
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

#Taken from nss_db library

# To mimmick the old glibc installation as closely as possible, we
# shuffle the installed library and the links to it around a bit,
# removing all traces from libtool in the process.
#install-data-local:
#	$(mkinstalldirs) $(DESTDIR)$(libdir)
#	rm $(DESTDIR)$(slibdir)/libnss_elektra.la
#	rm $(DESTDIR)$(slibdir)/libnss_elektra.so
#	mv $(DESTDIR)$(slibdir)/libnss_registry.so.$(INTERFACE).0.0 \
#	$(DESTDIR)$(slibdir)/libnss_registry-$(VERSION).so
#	rm $(DESTDIR)$(slibdir)/libnss_registry.so.$(INTERFACE)
#	$(top_srcdir)/rellns-sh $(DESTDIR)$(slibdir)/libnss_registry-$(VERSION).so \
#		$(DESTDIR)$(slibdir)/libnss_registry.so.$(INTERFACE)
#	rm -f $(DESTDIR)$(libdir)/libnss_registry.so
#	$(top_srcdir)/rellns-sh \
#		$(DESTDIR)$(slibdir)/libnss_registry.so.$(INTERFACE) \
#		$(DESTDIR)$(libdir)/libnss_registry.so

