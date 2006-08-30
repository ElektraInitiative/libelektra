# $Id$

EXTRA_DIST=kdb.1.xml elektra.7.xml elektra.5.xml
EXTRA_DIST+=author.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml overview.xml apiexample.xml api.xml compared.xml garbage.xml html-params.xsl html-titlepage-layout.tpl society.xml storage.xml UPLOAD
EXTRA_DIST+=docbook.css doxygen.css
#EXTRA_DIST+=elektra.odp
EXTRA_DIST+=$(all_mans)
EXTRA_DIST+= $(HTML_TITLE_LAYOUT_XSL)

SUBDIRS=images standards

# Dependencies for Doxygen documentation
# main...
DOXYSOURCES=$(srcdir)/../src/libelektra/kdb.c $(srcdir)/../src/include/kdb.h $(srcdir)/../src/libelektra/key.c $(srcdir)/../src/libelektra/keyset.c $(srcdir)/../src/libelektra/kdbhighlevel.c $(srcdir)/../src/libelektra/backendhelpers.c $(srcdir)/../src/Doxyfile

# tools...
DOXYSOURCES+=$(srcdir)/../src/libelektratools/kdbtools.c

# kdb command...
DOXYSOURCES+=$(srcdir)/../src/kdb/kdb.c

# backend stuff...
DOXYSOURCES+= $(srcdir)/../src/include/kdbbackend.h ../src/include/kdbprivate.h $(srcdir)/../src/backends/template/template.c


man3_MANS =
# man pages are installed in any case, but not distributed if they
# annot be built
man1_MANS = kdb.1
man5_MANS = elektra.5
man7_MANS = elektra.7


# 3 xml files built from source files. Not used for now
csource.xml: $(srcdir)/../examples/example.c
	(echo "<programlisting><![CDATA["; cat $<; echo "]]></programlisting>") > csource.xml

xorgpatch.xml: patches/xorg/README.xorg.Elektra.txt
	(echo "<screen><![CDATA["; cat  $<; echo "]]></screen>") > xorgpatch.xml

initpatch.xml: patches/sysvinit/README.sysvinit.Elektra.txt
	(echo "<screen><![CDATA["; cat  $<; echo "]]></screen>") > initpatch.xml


if HAVE_XSL

SUFFIXES: .xml .html .pdf .ps .rtf .xsl .fo .tpl
#
## Build man documentation
#
all_mans = $(man1_MANS) $(man5_MANS) $(man7_MANS)

kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<
	
elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<
	
elektra.5: elektra.5.xml author.xml storage.xml
	$(xsltproc) $(dbroot)/manpages/docbook.xsl $<


#
# HTML and doc targets for the previous web site. Obsolete, unused for now
#

# howto.html dependencies and target (unused)
.xml.html:
	$(xsltproc) -path . -o $@ ${HTML_TITLE_LAYOUT_XSL} $<

howto.html: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml api.xml society.xml compared.xml api.xml apiexample.xml xorgpatch.xml initpatch.xml csource.xml ${HTML_TITLE_LAYOUT_XSL}

# Layout and configurations to generate HTML for the web site
HTML_TITLE_LAYOUT_XSL=html-titlepage-layout.xsl
HTML_TITLE_LAYOUT=html-titlepage-layout.tpl

$(HTML_TITLE_LAYOUT_XSL): $(HTML_TITLE_LAYOUT)
	-$(xsltproc) -o ${HTML_TITLE_LAYOUT_XSL} $(dbroot)/template/titlepage.xsl $(srcdir)/${HTML_TITLE_LAYOUT}

# web site
libelektra.org: howto.xml elektra.7.xml author.xml garbage.xml overview.xml bestpract.xml rgexample.xml rgcmd.xml storage.xml api.xml society.xml compared.xml api.xml apiexample.xml xorgpatch.xml initpatch.xml csource.xml elektra-api.tar.gz $(man_html) ${HTML_TITLE_LAYOUT_XSL}
	$(xsltproc) -path . -stringparam base.dir $@ ${HTML_TITLE_LAYOUT_XSL} $<
	-cp $(srcdir)/docbook.css $@/
	-mkdir $@/images/
	-cp -r images/*png images/*gif $@/images
	-find elektra-api/html | cpio -pdvm $@
	-cp elektra-api.tar.gz $@
	-cp $(man_html) $@

else
# in case there are no man pages in source dir and the xsl tools
# are not installed, make install will fail without the files.
# this situation should be very rare, it could only happen
# from cvs (and without xsl tools) or after a make maintainer-clean
# or after a modification of the xml sources.
kdb.1: kdb.1.xml author.xml bestpract.xml rgexample.xml rgcmd.xml
	touch $<
	
elektra.7: elektra.7.xml author.xml overview.xml bestpract.xml rgexample.xml
	touch $<
	
elektra.5: elektra.5.xml author.xml storage.xml
	touch $<


endif

# html generated from man pages.
if HAVE_MAN2HTML
man_html = kdb.1.html elektra.7.html elektra.5.html kdb.3.html key.3.html keyset.3.html

kdb.1.html: kdb.1
	$(man2html) -r $< | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.1.html

elektra.5.html: elektra.5
	$(man2html) -r $< | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.5.html

elektra.7.html: elektra.7
	$(man2html) -r $< | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > elektra.7.html

kdb.3.html: elektra-api
	dir=.; \
	if test -f elektra-api/man/man3/kdb.3; then dir=elektra-api/man/man3 ;\
	else \
	  if test -f $(srcdir)/elektra-api/man/man3/kdb.3; then dir=$(srcdir)/elektra-api/man/man3 ; fi; \
	fi; \
	$(man2html) -r < $$dir/kdb.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > kdb.3.html

key.3.html: elektra-api
	dir=.; \
	if test -f elektra-api/man/man3/key.3; then dir=elektra-api/man/man3 ;\
	else \
	  if test -f $(srcdir)/elektra-api/man/man3/key.3; then dir=$(srcdir)/elektra-api/man/man3 ; fi; \
	fi; \
	$(man2html) -r < $$dir/key.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > key.3.html

keyset.3.html: elektra-api
	dir=.; \
	if test -f elektra-api/man/man3/keyset.3; then dir=elektra-api/man/man3 ;\
	else \
	  if test -f $(srcdir)/elektra-api/man/man3/keyset.3; then dir=$(srcdir)/elektra-api/man/man3 ; fi; \
	fi; \
	$(man2html) -r < $$dir/keyset.3 | sed -e 's|Content-type: text/html||; s|\"\.\.\/man.\/|\"|g; s|\"\.\.\/|\"|g;' > keyset.3.html
endif


# all: documentation

# html doxygen
# documentation: docbookman doxygen 
# docbookman: kdb.1 elektra.7 elektra.5
	
develdoc_DATA = elektra-api
apihtmldir = $(develdocdir)/api-html

if HAVE_DOXYGEN
#
# Generate doxygen documentation
# This generate man for API too (see src/Doxyfile)
#
elektra-api.tar.gz: elektra-api
	tar -zcf elektra-api.tar.gz elektra-api/html

elektra-api: $(DOXYSOURCES)
	@echo "Making Doxygen..."
	cd ../src/; srcdir=$(top_srcdir)/src $(doxygen) $(top_srcdir)/src/Doxyfile

clean-local:
	-rm -rf elektra-api
	-rm -rf libelektra.org


else
# fake target that is required when doxygen isn't found and the directory
# has to be created (make install, make dist)
elektra-api:
	mkdir elektra-api

clean-local:
	rmdir elektra-api

endif

CLEANFILES = csource.xml \
	$(man_html) \
	$(HTML_TITLE_LAYOUT_XSL) \
	xorgpatch.xml initpatch.xml \
	elektra-api.tar.gz

MAINTAINERCLEANFILES = howto.html \
	kdb.1 elektra.5 elektra.7 

# putting in EXTRA_DIST doesn't work, since in that case it may be 
# copied from srcdir and also from the current dir, leading to an error 
dist-hook:
	if test -d elektra-api/html; \
	  then cp -pR elektra-api $(distdir); \
	else \
	  if test -d $(srcdir)/elektra-api; \
	    then cp -pR $(srcdir)/elektra-api $(distdir); \
	  fi; \
	fi

install-develdocDATA: $(api_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(apihtmldir)
	@if test -d elektra-api/html/; then api_d=elektra-api/html/; \
	  else api_d=$(srcdir)/elektra-api/html/; \
	fi; \
	list=`find $$api_d`; for p in $$list; do \
	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apihtmldir)/$$f"; \
	  $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apihtmldir)/$$f; \
	done

uninstall-develdocDATA:
	-if test -d elektra-api/html/; then api_d=elektra-api/html/; \
	  else api_d=$(srcdir)/elektra-api/html/; \
	fi; \
	list=`find elektra-api/html/`; for p in $$list; do \
	f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo "rm -f $(DESTDIR)$(apihtmldir)/$$f"; \
	  rm -f $(DESTDIR)$(apihtmldir)/$$f; \
	 done
	-rmdir $(DESTDIR)$(apihtmldir)

#	$(mkinstalldirs) $(DESTDIR)$(apimandir)
#	@list=`find elektra-api/man/man3`; for p in $$list; do \
#	if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
#	f="`echo $$p | sed -e 's|^.*/||'`"; \
#	  echo " $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apimandir)/$$f"; \
#	  $(develdocDATA_INSTALL) $$d$$p $(DESTDIR)$(apimandir)/$$f; \
#	done

install-man3: $(man3_MANS)
	@$(NORMAL_INSTALL)
	test -z "$(man3dir)" || $(mkdir_p) "$(DESTDIR)$(man3dir)"
	@if test -d elektra-api/man/; then man_d=elektra-api/man/; \
	  else man_d=$(srcdir)/elektra-api/man/; \
	fi; \
	list=`find $$man_d -name "k[db,ey,s]*.3"` ; for i in $$list; do \
	if test -f "$$i"; then d=; else d="$(srcdir)/"; fi; \
	  ext='3'; \
	  inst=`echo $$i | sed -e 's/\\.[0-9a-z]*$$//'`; \
	  inst=`echo $$inst | sed -e 's/^.*\///'`; \
	  inst=`echo $$inst | sed '$(transform)'`.$$ext; \
	  echo " $(INSTALL_DATA) $$d$$i $(DESTDIR)$(man3dir)/$$inst"; \
	  $(INSTALL_DATA) $$d$$i $(DESTDIR)$(man3dir)/$$inst; \
	done


uninstall-man3:
	@$(NORMAL_UNINSTALL)
	-if test -d elektra-api/man/; then man_d=elektra-api/man/; \
	  else man_d=$(srcdir)/elektra-api/man/; \
	fi; \
	list=`find $$man_d -name "k[db,ey,s]*.3"` ; for i in $$list; do \
	  ext='3'; \
	  inst=`echo $$i | sed -e 's/\\.[0-9a-z]*$$//'`; \
	  inst=`echo $$inst | sed -e 's/^.*\///'`; \
	  inst=`echo $$inst | sed '$(transform)'`.$$ext; \
	  echo " rm -f '$(DESTDIR)$(man3dir)/$$inst'"; \
	  rm -f "$(DESTDIR)$(man3dir)/$$inst"; \
	done

