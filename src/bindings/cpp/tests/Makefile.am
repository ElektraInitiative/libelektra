INCLUDES = -I$(top_srcdir)/src/include -I$(top_srcdir)/src/bindings/cpp/include
AM_CXXFLAGS= $(CXXOPTFLAGS) $(CXXDBGFLAGS)

TESTS=test_key test_ks test_kdb

check_PROGRAMS=test_key test_ks test_kdb

if VALGRINDTESTS
TESTS_ENVIRONMENT = $(VALGRIND) --quiet --show-reachable=yes --leak-check=yes
endif

test_key_SOURCES = test_key.cpp tests.h tests.cpp
test_key_LDADD = $(privatelibs) ../libelektra-cpp.a ../../../libelektra/libelektra.a

test_ks_SOURCES = test_ks.cpp tests.h tests.cpp
test_ks_LDADD = $(privatelibs) ../libelektra-cpp.a ../../../libelektra/libelektra.a

test_kdb_SOURCES = test_kdb.cpp tests.h tests.cpp
test_kdb_LDADD = $(privatelibs) ../libelektra-cpp.a ../../../libelektra/libelektra.a

../libelektra-cpp.a:
	cd .. && $(MAKE) libelektra-cpp.a

../../../libelektra/libelektra.a:
	cd ../../../libelektra && $(MAKE) libelektra.a

clean-local:
	rm -rf .kdb
	rm -f *.gcno *.gcda *.gcno
