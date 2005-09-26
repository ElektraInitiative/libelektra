
# $Id$
# $LastChangedBy$

NAME=elektra
LIBNAME=libelektra
DTDVERSION=0.1.1

LIBVERSION=1.0     # used as ${LIBNAME}.so.$(LIBVERSION)

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
CC=gcc -I. -g ${OPTIMIZATIONS} -Wall
#CC=gcc -g ${OPTIMIZATIONS} -Wcomment -Wformat -Wimplicit-int -Wimplicit-function-declaration -Wparentheses -Wreturn-type -Wunused -Wuninitialized
XMLINCLUDES=`xml2-config --cflags`
XMLLIBS=`xml2-config --libs`
DEFAULT_BACKEND=filesys


.c.o:
	${CC} -fpic -o $@ -c $<



all: ${LIBNAME}.a ${LIBNAME}.so ${LIBNAME}tools.so kdb     # kdbfuture.h
	for x in ${BACKENDS}; do (cd "backends/$$x"; $(MAKE) \
		OPTIMIZATIONS="${OPTIMIZATIONS}" \
		DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); \
	done

clean:
	-rm -f core* *~ *.o *.so* *.a kdb kdbfuture.h localeinfo svn-commit*
	-find . -name "*~" -o -name ".kdbg*" | xargs rm -f
	for x in ${BACKENDS}; do (cd "backends/$$x"; $(MAKE) $@); done


distclean: clean


libregistry.so: registrystub.o ${LIBNAME}.so
	${CC} -shared -fpic -o $@ ${LIBNAME}.so.$(LIBVERSION) registrystub.o


${LIBNAME}.so: libkdb.o key.o keyset.o
	${CC} -shared -fpic -ldl -Wl,-soname,$@.$(LIBVERSION) -o $@.$(LIBVERSION) libkdb.o key.o keyset.o
	ln -sf $@.$(LIBVERSION) $@



${LIBNAME}.a: libkdb.o key.o keyset.o
	ar q $@ libkdb.o key.o keyset.o



${LIBNAME}tools.so: kdbtools.o
	${CC} -shared -fpic ${XMLLIBS} -ldl -Wl,-soname,$@.$(LIBVERSION) -o $@.$(LIBVERSION) kdbtools.o
	ln -sf $@.$(LIBVERSION) $@



${LIBNAME}tools.a: kdbtools.o
	ar q $@ kdbtools.o




	
	
key.o: key.c kdb.h kdbprivate.h

keyset.o: keyset.c kdb.h kdbprivate.h



libkdb.o: libkdb.c kdb.h kdbprivate.h


kdbtools.o: kdbtools.c kdbtools.h kdb.h
	${CC} ${XMLINCLUDES} -fpic -c $<



kdb.o: kdb.c kdb.h kdbprivate.h
#	${CC} ${XMLINCLUDES} -fpic -c $<



kdb: kdb.o ${LIBNAME}.so
	${CC} -DKDB_SCHEMA_PATH="${SGMLDIR}/elektra.xsd" ${LIBNAME}.so -o $@ $<


kdbfuture.h: kdb.h
	grep -v ' _DEPRECATED_ ' $< > $@


install: all
	# Strips will be done automatically by rpmbuild
	# strip ${LIBNAME}.so
	# strip libregistry.so  # remove me in the future
	# strip kdb
	cp -d ${LIBNAME}.so* ${DESTDIR}${LIBDIR}
	cp -d ${LIBNAME}tools.so* ${DESTDIR}${ULIBDIR}
	cp ${LIBNAME}.a ${DESTDIR}${ULIBDIR}
	#cp libregistry.so ${DESTDIR}${LIBDIR} # remove me in the future
	cp kdb ${DESTDIR}${BINDIR}
	cp kdb.h kdbprivate.h kdbbackend.h kdbtools.h ${DESTDIR}${INCDIR}
	cp kdb.c ${DESTDIR}${DOCDIR}/${NAME}-devel/examples/
	# cp kdbfuture.h ${DESTDIR}${DOCDIR}/${NAME}-devel
	cp -r backends/template ${DESTDIR}${DOCDIR}/${NAME}-devel/backend-template
	for x in ${BACKENDS}; do (cd "backends/$$x"; $(MAKE) DESTDIR=${DESTDIR}\
		OPTIMIZATIONS="${OPTIMIZATIONS}" \
		DTDVERSION=${DTDVERSION} \
		DOCDIR=${DOCDIR} MANDIR=${MANDIR} SGMLDIR=${SGMLDIR} \
		BINDIR=${BINDIR} LIBDIR=${LIBDIR} \
		UBINDIR=${UBINDIR} ULIBDIR=${ULIBDIR} \
		CONFDIR=${CONFDIR} INCDIR=${INCDIR} $@); \
	done
	ln -s ${LIBNAME}-${DEFAULT_BACKEND}.so ${DESTDIR}/${LIBDIR}/${LIBNAME}-default.so

