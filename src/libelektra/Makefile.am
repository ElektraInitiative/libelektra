
# $Id$

lib_LIBRARIES = libelektra.a
libelektra_a_SOURCES = libkdb.c key.c keyset.c backend_symbols.c \
		       ../include/kdb.h \
		       ../include/kdbbackend.h \
		       ../include/kdbprivate.h \
		       ../include/kdbLibLoader.h
libelektra_a_CFLAGS = -D__STATIC -DDEFAULT_BACKEND=\"@default_backend@\"
libelektra_a_LIBADD =  ../libloader/libloader_static_a-kdbLibLoader.o \
		       ../backends/filesys/libelektra_filesys_a-filesys.o \
		       $(LIBICONV)

lib_LTLIBRARIES = libelektra.la
libelektra_la_SOURCES = libkdb.c key.c keyset.c ../include/kdb.h ../include/kdbbackend.h ../include/kdbprivate.h
libelektra_la_CFLAGS = -DDEFAULT_BACKEND=\"default\"
libelektra_la_LDFLAGS = -version-info 1:0:0 
libelektra_la_LIBADD = $(LIBLTDL) $(LIBICONV) ../libloader/kdbLibLoader.lo


INCLUDES = -I../include $(LTDLINCL)
