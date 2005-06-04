# $Id$
# $LastChangedBy$

# This is a Makefile for the template backend.
# You probably will have to change these 2 next vars to match your sources.

BACKENDNAME=gconf
SOURCES=${BACKENDNAME}.c




# Usually nothing else to change under this point


OPTIMIZATIONS=-O
CC=gcc -g ${OPTIMIZATIONS} -Wall


.c.o:
	${CC} -I../.. -fpic -o $@ -c $<


all: libkdb-${BACKENDNAME}.so



libkdb-${BACKENDNAME}.so: ${SOURCES:.c=.o}
	${CC} `pkg-config --libs gconf-2.0` -shared -fpic -L../.. -lkdb -o $@ $<




${BACKENDNAME}.o: ${BACKENDNAME}.c
	${CC} `pkg-config --cflags gconf-2.0` -I../.. -fpic -o $@ -c $<




install: all
	cp -d libkdb-${BACKENDNAME}.so ${DESTDIR}${LIBDIR}




clean:
	-rm -f ${SOURCES:.c=.o} libkdb-${BACKENDNAME}.so
	-find . -name "*~" -o -name ".kdbg*" | xargs rm -f


