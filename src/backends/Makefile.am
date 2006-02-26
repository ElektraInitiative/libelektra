
# $Id$

SUBDIRS = filesys fstab 

if EXPERIMENTAL
SUBDIRS +=gconf template ini daemon berkeleydb
endif

install-exec-hook:
	cd $(DESTDIR)$(libdir) && \
	test -e libelektra-default.so || \
	$(LN_S) libelektra-@default_backend@.so libelektra-default.so 
