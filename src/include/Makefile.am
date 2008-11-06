
# $Id$

nobase_include_HEADERS = kdb.h kdbloader.h kdbbackend.h kdbtools.h kdbprivate.h kdbos.h

EXTRA_DIST = kdbprivate.h.in kdb.h.in

kdbprivate.h: kdbprivate.h.in ../../config.status
	sed -e "s:[@]KDB_DB_SYSTEM[@]:$(sysconfdir)/kdb:" "$(srcdir)/kdbprivate.h.in" > kdbprivate.h

kdb.h: kdb.h.in ../../config.status
	sed -e "s:[@]KDB_VERSION[@]:\"$(KDB_VERSION)\":" "$(srcdir)/kdb.h.in" |\
	sed -e "s:[@]KDB_VERSION_MAJOR[@]:$(KDB_VERSION_MAJOR):" |\
	sed -e "s:[@]KDB_VERSION_MINOR[@]:$(KDB_VERSION_MINOR):" |\
	sed -e "s:[@]KDB_VERSION_MICRO[@]:$(KDB_VERSION_MICRO):" > kdb.h

DISTCLEANFILES = kdbprivate.h kdb.h

MAINTAINERCLEANFILES = \
	Makefile.in \
	config.h.in
