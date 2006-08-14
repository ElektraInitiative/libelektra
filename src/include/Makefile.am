
# $Id$

nobase_include_HEADERS = kdb.h kdbbackend.h kdbtools.h kdbprivate.h

EXTRA_DIST = kdbprivate.h.in

kdbprivate.h: kdbprivate.h.in ../../config.status
	sed -e "s:[@]KDB_DB_SYSTEM[@]:$(sysconfdir)/kdb:" $< > kdbprivate.h

DISTCLEANFILES = kdbprivate.h
