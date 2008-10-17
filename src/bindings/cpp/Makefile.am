INCLUDES = -I$(top_srcdir)/src/include -I$(top_srcdir)/src/bindings/cpp/include
AM_CXXFLAGS = $(CXXSTDFLAGS) $(CXXOPTFLAGS) $(CXXDBGFLAGS)

SUBDIRS = tests
DIST_SUBDIRS = tests

include_HEADERS = include/kdb include/key include/keyset

dependencies = ../../include/kdb.h

sources =  kdb.cpp key.cpp keyset.cpp include/kdb include/key include/keyset

objects = kdb.o key.o keyset.o

lib_LIBRARIES = libelektra-cpp.a
libelektra_cpp_a_SOURCES = $(sources)
libelektra_cpp_a_DEPENDENCIES =
libelektra_cpp_a_CXXFLAGS = $(cxxflags)
libelektra_cpp_a_CPPFLAGS = -DELEKTRA_STATIC $(cppflags)
libelektra_cpp_a_LIBADD =
BUILT_SOURCES =

lib_LTLIBRARIES = libelektra-cpp.la
libelektra_cpp_la_SOURCES = $(sources)
libelektra_cpp_la_DEPENDENCIES =
libelektra_cpp_la_CXXFLAGS = $(cxxflags)
libelektra_cpp_la_CPPFLAGS = $(cppflags)
libelektra_cpp_la_LDFLAGS = -version-info 0:0:0
libelektra_cpp_la_LIBADD = ../../libelektra/libelektra.la

clean-local:
	rm -f *.gcno *.gcda *.gcno
