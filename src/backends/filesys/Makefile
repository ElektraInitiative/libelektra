# $Id$
# $LastChangedBy$



BACKENDNAME=filesys
LIBNAME=libelektra
SOURCES=${BACKENDNAME}.c




# Usually nothing else to change under this point


OPTIMIZATIONS=-O
CC=gcc -g ${OPTIMIZATIONS} -Wall


.c.o:
	${CC} -I../.. -fpic -o $@ -c $<


all: ${LIBNAME}-${BACKENDNAME}.so



${LIBNAME}-${BACKENDNAME}.so: ${SOURCES:.c=.o}
	${CC} -shared -fpic -lelektra -L../../ -o $@ $<




${BACKENDNAME}.o: ${BACKENDNAME}.c
	${CC} -I../.. -fpic -o $@ -c $<




install: all
	cp -d ${LIBNAME}-${BACKENDNAME}.so ${DESTDIR}${LIBDIR}




clean:
	-rm -f ${SOURCES:.c=.o} ${LIBNAME}-${BACKENDNAME}.so
	-find . -name "*~" -o -name ".kdbg*" | xargs rm -f


