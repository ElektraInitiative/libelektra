# $Id$
# $LastChangedBy$



BACKENDNAME=filesys
SOURCES=${BACKENDNAME}.c




# Usually nothing else to change under this point


OPTIMIZATIONS=-O
CC=gcc -g ${OPTIMIZATIONS} -Wall


.c.o:
	${CC} -I../.. -fpic -o $@ -c $<


all: libkdb-${BACKENDNAME}.so



libkdb-${BACKENDNAME}.so: ${SOURCES:.c=.o}
	${CC} -shared -fpic -lkdb -L../../ -o $@ $<




${BACKENDNAME}.o: ${BACKENDNAME}.c
	${CC} -I../.. -fpic -o $@ -c $<




install: all
	cp -d libkdb-${BACKENDNAME}.so ${DESTDIR}${LIBDIR}




clean:
	rm -f ${SOURCES:.c=.o} libkdb-${BACKENDNAME}.so
	-find . -name "*~" -o -name ".kdbg*" | xargs rm -f


