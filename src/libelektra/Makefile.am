
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

lib_LIBRARIES = libelektra.a
libelektra_a_SOURCES = kdb.c error.c key.c keyset.c kdbhighlevel.c backendhelpers.c \
		../backends/backend_symbols.c\
		../include/kdb.h \
		../include/kdbbackend.h \
		../include/kdbprivate.h \
		../include/kdbLibLoader.h
libelektra_a_CFLAGS = -DELEKTRA_STATIC -DDEFAULT_BACKEND=\"$(default_backend)\"
libelektra_a_LIBADD =  ../libloader/libloader_static_a-kdbLibLoader.$(OBJEXT)
libelektra_a_LIBADD += `cat ../backends/objects`
libelektra_a_DEPENDENCIES = ../backends/objects ../libloader/libloader-static.a

if EXPERIMENTAL
libelektra_a_CFLAGS += -DEXPERIMENTAL
libelektra_a_LIBADD += ../backends/ini/libelektra_ini_a-ini.$(OBJEXT) \
                       ../backends/ini/libelektra_ini_a-helpers.$(OBJEXT) \
		       ../backends/ini/libelektra_ini_a-parser.$(OBJEXT)
endif

lib_LTLIBRARIES = libelektra.la
libelektra_la_SOURCES = kdb.c error.c key.c keyset.c kdbhighlevel.c backendhelpers.c \
		../include/kdb.h \
		../include/kdbbackend.h \
		../include/kdbprivate.h
libelektra_la_CFLAGS = -DDEFAULT_BACKEND=\"default\"
libelektra_la_LDFLAGS = -version-info 2:1:0
libelektra_la_LIBADD = $(LIBICONV) ../libloader/libloader-dynamic.la

../libloader/libloader-static.a:
	cd ../libloader && $(MAKE) libloader-static.a

../libloader/libloader-dynamic.la:
	cd ../libloader && $(MAKE) libloader-dynamic.la
	
../backends/backend_symbols.c:
	cd ../backends && $(MAKE) symbols
	
../backends/objects:
	cd ../backends && $(MAKE) objects
