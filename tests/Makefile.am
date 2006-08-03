AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

TESTS = test_key test_kdb

check_PROGRAMS = test_key test_kdb

EXTRA_DIST = keyset.xml

test_key_SOURCES = test_key.c
test_key_LDADD = $(privatelibs) ../src/libelektra/libelektra.a 

test_kdb_SOURCES = test_kdb.c
test_kdb_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

../src/libelektra/libelektra.a:
	cd ../src/libelektra && $(MAKE) libelektra.a

