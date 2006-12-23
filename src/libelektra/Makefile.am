
# $Id$

AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

elektra_sources = kdb.c error.c key.c keyset.c kdbhighlevel.c backendhelpers.c

elektra_dependencies = ../include/kdbprivate.h \
	../include/kdb.h \
	../include/kdbbackend.h

lib_LIBRARIES = libelektra.a
libelektra_a_SOURCES = $(elektra_sources) \
		exported_symbols.c \
		../include/kdbLibLoader.h
libelektra_a_DEPENDENCIES = $(elektra_dependencies) objects
libelektra_a_CFLAGS = -DELEKTRA_STATIC -DDEFAULT_BACKEND=\"$(default_backend)\"
libelektra_a_LIBADD = `cat objects`
BUILT_SOURCES = exported_symbols.c

#if EXPERIMENTAL
#libelektra_a_LIBADD += ../backends/ini/libelektra_ini_a-ini.$(OBJEXT) \
#                       ../backends/ini/libelektra_ini_a-helpers.$(OBJEXT) \
#		       ../backends/ini/libelektra_ini_a-parser.$(OBJEXT)
#endif

lib_LTLIBRARIES = libelektra.la
libelektra_la_SOURCES = $(elektra_sources)
libelektra_la_DEPENDENCIES = $(elektra_dependencies) ../libloader/libloader-dynamic.la
libelektra_la_CFLAGS = -DDEFAULT_BACKEND=\"default\"
libelektra_la_LDFLAGS = -version-info 2:2:0
libelektra_la_LIBADD = ../libloader/libloader-dynamic.la $(LIBICONV)

EXTRA_DIST = exportsymbols.sh

../include/kdbprivate.h:
	cd ../include && $(MAKE) kdbprivate.h

../libloader/libloader-static.a:
	cd ../libloader && $(MAKE) libloader-static.a

../libloader/libloader-dynamic.la:
	cd ../libloader && $(MAKE) libloader-dynamic.la
	
../libelektratools/libelektratools.a:
	cd ../libelektratools && $(MAKE) libelektratools.a
	
#all_backends:
#	cd ../backends && $(MAKE) all

../backends/filesys/libelektra-filesys.a:
	cd ../backends/filesys && $(MAKE) libelektra-filesys.a

../backends/ini/libelektra-ini.a:
	cd ../backends/ini && $(MAKE) libelektra-ini.a

../backends/fstab/libelektra-fstab.a:
	cd ../backends/fstab && $(MAKE) libelektra-fstab.a

../backends/daemon/libelektra-daemon.a:
	cd ../backends/daemon && $(MAKE) libelektra-daemon.a

../backends/berkeleydb/libelektra-berkeleydb.a:
	cd ../backends/berkeleydb && $(MAKE) libelektra-berkeleydb.a

../backends/gconf/libelektra-gconf.a:
	cd ../backends/gconf && $(MAKE) libelektra-gconf.a

#objects: ../libloader/libloader-static.a all_backends ../libelektratools/libelektratools.a
#	@list='$(backends)'; for backend in $$list; do \
#	  bck_list=$$bck_list" ../backends/$$backend/libelektra-$$backend.a "; \
#	done; \
#	$(SHELL) exportobjects.sh ../libloader/libloader-static.a $$bck_list ../libelektratools/libelektratools.a

object_deps = ../libloader/libloader-static.a $(backend_static_libs) ../libelektratools/libelektratools.a

objects: $(object_deps)
	$(SHELL) exportobjects.sh $(object_deps)

exported_symbols.c: ../../config.status
	$(SHELL) $(srcdir)/exportsymbols.sh $(backends) $(elektratools)

#exported_symbols.c: all_backends ../libelektratools/libelektratools.a
#	@list='$(backends)'; for backend in $$list; do \
#	  bck_list=$$bck_list" ../backends/$$backend/libelektra-$$backend.a"; \
#	done; \
#	$(SHELL) exportsymbols.sh $$bck_list ../libelektratools/libelektratools.a
clean-local:
	-rm -f objects
	-rm -f exported_symbols.c
