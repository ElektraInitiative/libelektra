###############################################################################
# Nickel - a library for hierarchical maps and .ini files
# One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
# Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
# $Id: Makefile 349 2008-01-19 18:18:22Z chaz $
###############################################################################


# This Makefile may need help on non-GNU/Linux systems.  Contributions are
# welcome!  ...But no GNU autotools please.


# Standard configurable make options.
CC=gcc
LD=gcc
INSTALL=install

CFLAGS=-O2
LDFLAGS=

prefix=/usr/local
exec_prefix=${prefix}
libdir=${exec_prefix}/lib
includedir=${prefix}/include


# Release-specific stuff.
VER_MAJOR=1
VER_MINOR=1
VER_AGE=0


# Project-specific stuff.
LIBNAME=libnickel.so
SONAME=$(LIBNAME).$(VER_MAJOR)
NICKEL=$(SONAME).$(VER_MINOR).$(VER_AGE)

OBJS= \
	buf.o \
	hash.o \
	io.o \
	nickel.o

TESTS= \
	ni

DIST_FILES= \
	AUTHORS \
	ChangeLog \
	COPYING \
	INSTALL \
	README \
	include/bohr/ni.h \
	src/Makefile \
	src/buf.c \
	src/hash.c \
	src/internal.h \
	src/io.c \
	src/nickel.c \
	src/include/bohr/ds_hash.h \
	src/include/bohr/ds_str.h \
	src/include/bohr/ds_vector.h \
	src/tests/ni.c \
	src/tests/ni_parse_spaces_quotes.ini \
	src/tests/ni_parse_children.ini \
	src/tests/ni_parse_oddities.ini \
	src/tests/ni_big_oct.ini


# Rules.
all: $(NICKEL)
$(NICKEL): $(OBJS)
	$(LD) $(LDFLAGS) -shared -fPIC -Wl,-soname,$(SONAME) -o $@ $^
	rm -f $(SONAME)
	ln -s $(NICKEL) $(SONAME)
$(OBJS): %.o: %.c
	$(CC) $(CFLAGS) -Wall -std=c99 -fPIC -Iinclude -I../include -o $@ -c $<

buf.c: internal.h ../include/bohr/ni.h
hash.c: internal.h
io.c: internal.h ../include/bohr/ni.h
nickel.c: internal.h include/bohr/ds_hash.h ../include/bohr/ni.h
internal.h: include/bohr/ds_vector.h include/bohr/ds_str.h

clean:
	rm -f $(NICKEL) $(SONAME) $(OBJS) $(TESTS:%=tests/%) $(TESTS:%=tests/%.o)

tests: $(TESTS:%=tests/%)
	@RC=0; cd tests; for t in $(TESTS); do LD_LIBRARY_PATH=.. ./$$t || RC=1; done; exit $$RC

$(TESTS:%=tests/%): %: %.o $(NICKEL)
	$(LD) -o $@ $(NICKEL) $<
$(TESTS:%=tests/%.o): %.o: %.c
	$(CC) -I../include -o $@ -c $<
$(TESTS:%=tests/%.c): ../include/bohr/ni.h

install: $(NICKEL) ../include/bohr/ni.h
	$(INSTALL) -m 644 -D ../include/bohr/ni.h $(DESTDIR)$(includedir)/bohr/ni.h
	$(INSTALL) -s -m 755 -D $(NICKEL) $(DESTDIR)$(libdir)/$(NICKEL)
	cd $(DESTDIR)$(libdir); \
	rm -f $(SONAME); \
	ln -s $(NICKEL) $(SONAME)
	(ldconfig || true) > /dev/null 2>&1

uninstall:
	rm -f $(DESTDIR)$(includedir)/bohr/ni.h $(DESTDIR)$(libdir)/$(NICKEL) $(DESTDIR)$(libdir)/$(SONAME)
	(ldconfig || true) > /dev/null 2>&1

dist:
	cd ..; \
	rm -rf nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE); \
	mkdir nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE); \
	cp --parents $(DIST_FILES) nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE); \
	rm -f nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE).tar.gz; \
	tar -czf nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE).tar.gz nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE); \
	rm -rf nickel-$(VER_MAJOR).$(VER_MINOR).$(VER_AGE)

.PHONY: all clean tests install uninstall dist
