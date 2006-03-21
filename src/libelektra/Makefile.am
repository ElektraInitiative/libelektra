
# $Id$

lib_LIBRARIES = libelektra.a
libelektra_a_SOURCES = libkdb.c error.c key.c keyset.c backend_symbols.c \
		../include/kdb.h \
		../include/kdbbackend.h \
		../include/kdbprivate.h \
		../include/kdbLibLoader.h
libelektra_a_CFLAGS = -D__STATIC -DDEFAULT_BACKEND=\"@default_backend@\" -ansi -pedantic -Wall
libelektra_a_LIBADD =  ../libloader/libloader_static_a-kdbLibLoader.o \
		../backends/filesys/libelektra_filesys_a-filesys.o \
		$(LIBICONV)
if EXPERIMENTAL
libelektra_a_CFLAGS += -DEXPERIMENTAL
libelektra_a_LIBADD += ../backends/ini/libelektra_ini_a-ini.o \
                       ../backends/ini/libelektra_ini_a-helpers.o \
		       ../backends/ini/libelektra_ini_a-parser.o
endif

lib_LTLIBRARIES = libelektra.la
libelektra_la_SOURCES = libkdb.c error.c key.c keyset.c \
		../include/kdb.h \
		../include/kdbbackend.h \
		../include/kdbprivate.h
libelektra_la_CFLAGS = -DDEFAULT_BACKEND=\"default\" -ansi -pedantic -Wall
libelektra_la_LDFLAGS = -version-info 2:0:0
libelektra_la_LIBADD = $(LIBLTDL) $(LIBICONV) ../libloader/kdbLibLoader.lo


INCLUDES = -I../include $(LTDLINCL)
