
.SUFFIXES: .dtd

all:

clean:
	-rm *~

distclean: clean

install:
	mkdir -p ${DESTDIR}/usr/share/sgml/elektra-${DTDVERSION}/
	cp *dtd catalog ${DESTDIR}/usr/share/sgml/elektra-${DTDVERSION}/


pub: all
	find elektra.dtd catalog | cpio -o | ssh -l aviram shell.sf.net "cd /home/groups/e/el/elektra/htdocs; [ -d dtd ] || mkdir dtd; cd dtd; cpio -idvm"

