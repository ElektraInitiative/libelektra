# $Id$

EXTRA_DIST=kdb.1.xml elektra.7.xml elektra.5.xml
EXTRA_DIST+=author.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml overview.xml apiexample.xml api.xml compared.xml garbage.xml html-params.xsl html-titlepage-layout.tpl society.xml storage.xml UPLOAD
EXTRA_DIST+=docbook.css doxygen.css
EXTRA_DIST+=elektra.odp
EXTRA_DIST+=$(man1_MANS) $(man5_MANS) $(man7_MANS)

SUBDIRS=images standards

# Dependencies for Doxygen documentation
# main...
DOXYSOURCES=../src/libelektra/kdb.c ../src/include/kdb.h ../src/libelektra/key.c ../src/libelektra/keyset.c ../src/libelektra/kdbhighlevel.c ../src/libelektra/backendhelpers.c ../src/Doxyfile

# tools...
DOXYSOURCES+=../src/libelektratools/kdbtools.c

# kdb command...
DOXYSOURCES+=../src/kdb/kdb.c

# backend stuff...
DOXYSOURCES+= ../src/include/kdbbackend.h ../src/include/kdbprivate.h ../src/backends/template/template.c


man3_MANS =

if HAVE_XSLTPROC
if HAVE_STYLESHEET



SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl

#
# TARGETS
#
.xml.html:
	$(xsltproc) -o ${HTML_TITLE_LAYOUT:.tpl=.xsl} $(dbroot)/template/titlepage.xsl ${HTML_TITLE_LAYOUT}
	$(xsltproc) -o $@ ${HTML_TITLE_LAYOUT:.tpl=.xsl} $<



man1_MANS = kdb.1
man5_MANS = elektra.5
man7_MANS = elektra.7

all_MANS = ${man1_MANS} ${man5_MANS} ${man7_MANS}


# Layout and confiruations to generate HTML for the web site
HTML_TITLE_LAYOUT=html-titlepage-layout.tpl
HTML_PARAMS=html-params.xsl

PATCHSOURCES=patches/xorg/README.xorg.Elektra.txt patches/sysvinit/README.sysvinit.Elektra.txt



#
## Build man documentation
#
kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<
	
elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<
	
elektra.5: elektra.5.xml author.xml storage.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<




#
# Build HTML documentation
#

example.c:
	$(LN_S) ../examples/example.c .

csource.xml: example.c
	(echo "<programlisting><![CDATA["; cat $<; echo "]]></programlisting>") > $@

xorgpatch.xml: patches/xorg/README.xorg.Elektra.txt
	(echo "<screen><![CDATA["; cat  $<; echo "]]></screen>") > $@

initpatch.xml: patches/sysvinit/README.sysvinit.Elektra.txt
	(echo "<screen><![CDATA["; cat  $<; echo "]]></screen>") > $@




howto.html: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml api.xml society.xml compared.xml api.xml apiexample.xml xorgpatch.xml initpatch.xml csource.xml





libelektra.org: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml api.xml society.xml compared.xml api.xml apiexample.xml xorgpatch.xml initpatch.xml csource.xml elektra-api.tar.gz htmlman
	$(xsltproc) -o ${HTML_TITLE_LAYOUT:.tpl=.xsl} $(dbroot)/template/titlepage.xsl ${HTML_TITLE_LAYOUT}
	$(xsltproc) -stringparam base.dir $@/ ${HTML_TITLE_LAYOUT:.tpl=.xsl} $<
	-cp docbook.css $@
	-mkdir $@/images/
	-cp -r images/*png images/*gif $@/images
	-find elektra-api/html | cpio -pdvm $@
	-cp elektra-api.tar.gz $@
	-cp kdb.1.html elektra.7.html elektra.5.html kdb.3.html key.3.html keyset.3.html $@





htmlman: elektra-api ${all_MANS}
	$(man2html) -r < kdb.1 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.1.html
	$(man2html) -r < elektra.7 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.7.html
	$(man2html) -r < elektra.5 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.5.html
	$(man2html) -r < elektra-api/man/man3/kdb.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.3.html
	$(man2html) -r < elektra-api/man/man3/key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html
	$(man2html) -r < elektra-api/man/man3/keyset.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > keyset.3.html




endif
endif



# all: documentation

# html doxygen
# documentation: docbookman doxygen 
# docbookman: kdb.1 elektra.7 elektra.5
	
if HAVE_DOXYGEN
apimandir = $(develdocdir)/man/man3
apihtmldir = $(develdocdir)/api-html
develdoc_DATA = elektra-api

#
# Generate doxygen documentation
# This generate man for API too (see src/Doxyfile)
#

elektra-api.tar.gz: elektra-api
	tar -zcf elektra-api.tar.gz elektra-api/html

elektra-api: $(DOXYSOURCES)
	@echo "Making Doxygen..."
	cd ../src/; $(doxygen)

clean:
	-rm csource.xml
	-rm example.c
	-rm kdb.1 elektra.5 elektra.7
	-rm html-titlepage-layout.xsl
	-rm xorgpatch.xml
	-rm initpatch.xml
	-rm elektra-api.tar.gz
	-rm -rf elektra-api
	-rm -rf libelektra.org


install-develdocDATA: $(api_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(apihtmldir)
	@list=`find elektra-api/html/`; for p in $$list; do \
	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apihtmldir)/$$f"; \
	  $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apihtmldir)/$$f; \
	 done

#	$(mkinstalldirs) $(DESTDIR)$(apimandir)
#	@list=`find elektra-api/man/man3`; for p in $$list; do \
#	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
#	f="`echo $$p | sed -e 's|^.*/||'`"; \
#	  echo " $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apimandir)/$$f"; \
#	  $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apimandir)/$$f; \
#	done

	$(mkinstalldirs) $(DESTDIR)$(man3dir)
	@list=`find elektra-api/ -name "k[db,ey,s]*.3"` ; for p in $$list; do \
	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(man3dir)/$$f"; \
	  $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(man3dir)/$$f; \
	done
endif





