
# Default dirs we use for installation, that can be substituted by the command line.
SGMLDIR=/usr/share/sgml

.SUFFIXES: .dtd

all:

clean:
	-rm *~

distclean: clean

install:
	mkdir -p ${DESTDIR}${SGMLDIR}/elektra-${DTDVERSION}/
	cp *dtd catalog ${DESTDIR}${SGMLDIR}/elektra-${DTDVERSION}/


pub: all
	find elektra.dtd catalog | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; [ -d dtd ] || mkdir dtd; cd dtd; cpio -idvm"

