AM_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

#TESTS = large threads
#TESTS = large

#check_PROGRAMS = large threads
#check_PROGRAMS = large

if VALGRINDTESTS
TESTS_ENVIRONMENT = $(VALGRIND) --quiet --show-reachable=yes --leak-check=yes
else
TESTS_ENVIRONMENT = time -p
endif

EXTRA_DIST =

#threads_SOURCES = threads.c benchmarks.c benchmarks.h
#threads_LDADD = $(privatelibs) ../src/libelektra/libelektra.a -lpthread

#large_SOURCES = large.c benchmarks.c benchmarks.h
#large_LDADD = $(privatelibs) ../src/libelektra/libelektra.a 

../src/libelektra/libelektra.a:
	cd ../src/libelektra && $(MAKE) libelektra.a

clean-local:
	rm -rf .kdb
	rm -f *.gcno *.gcda *.gcno

