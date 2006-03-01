
# $Id$

SUBDIRS = filesys fstab berkeleydb

if EXPERIMENTAL
SUBDIRS +=gconf template ini daemon 
endif

install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
