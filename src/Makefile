
# $Id: Makefile 111 2004-12-04 14:11:47Z aviram $
# $LastChangedBy: aviram $

NAME=elektra
DTDVERSION=0.1.0

LIBVERSION=1  # for libkdb.so.$(LIBVERSION)

# Default dirs we use for installation, that can be substituted by the
# command line. See the README file or the RPM spec file.
#BINDIR=/bin
#UBINDIR=/usr/bin
#LIBDIR=/lib
#ULIBDIR=/usr/lib
#CONFDIR=/etc
#INCDIR=/usr/include
#DOCDIR=/usr/share/doc
#MANDIR=/usr/share/man
#SGMLDIR=/usr/share/sgml


OPTIMIZATIONS=-O
CC=gcc -g ${OPTIMIZATIONS} -Wall
#CC=gcc -g ${OPTIMIZATIONS} -Wcomment -Wformat -Wimplicit-int -Wimplicit-function-declaration -Wparentheses -Wreturn-type -Wunused -Wuninitialized
XMLINCLUDES=`xml2-config --cflags`
XMLLIBS=`xml2-config --libs`


.c.o:
	${CC} -fpic -o $@ -c $<



all: libkdb.a libkdb.so libregistry.so kdb kdbfuture.h


clean:
	-rm core* *~ *.o *.so* *.a kdb kdbfuture.h localeinfo svn-commit*
	-find . -name "*~" | xargs rm


distclean: clean


libregistry.so: registrystub.o libkdb.so
	${CC} -shared -fpic -o $@ libkdb.so.$(LIBVERSION) registrystub.o


libkdb.so: localkdb.o key.o
	${CC} -shared -fpic -Wl,-soname,$@.$(LIBVERSION) -o $@.$(LIBVERSION) localkdb.o key.o
	ln -sf $@.$(LIBVERSION) $@



libkdb.a: localkdb.o key.o
	ar q $@ localkdb.o key.o




key.o: key.c kdb.h kdbprivate.h
	${CC} -fpic -o $@ -c $<





localkdb.o: localkdb.c kdb.h kdbprivate.h
	${CC} -fpic -c $<




kdb.o: kdb.c kdb.h kdbprivate.h
	${CC} ${XMLINCLUDES} -fpic -c $<



kdb: kdb.o libkdb.so
	${CC} -Wl,-rpath,. -L. ${XMLLIBS} -lkdb -o $@ $<


kdbfuture.h: kdb.h
	grep -v ' _DEPRECATED_ ' $< > $@


install: all
	# Strips will be done automatically by rpmbuild
	# strip libkdb.so
	# strip libregistry.so  # remove me in the future
	# strip kdb
	cp -d libkdb.so* ${DESTDIR}${LIBDIR}
	cp libkdb.a ${DESTDIR}${ULIBDIR}
	cp libregistry.so ${DESTDIR}${LIBDIR} # remove me in the future
	cp kdb ${DESTDIR}${BINDIR}
	cp kdb.h ${DESTDIR}${INCDIR}
	cp kdb.c ${DESTDIR}${DOCDIR}/${NAME}-devel
	cp kdbfuture.h ${DESTDIR}${DOCDIR}/${NAME}-devel

