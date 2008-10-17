AM_CFLAGS = $(COPTFLAGS) $(CDBGFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/src/include $(LTDLINCL)

TESTS = test_key test_ks test_kdb test_xml test_validate.sh test_getset test_trie test_mount test_cap test_stream test_serialize test_split test_internals test_backendhelpers test_script.sh test_type
#TESTS = test_backendhelpers

check_PROGRAMS    = test_key test_ks test_kdb test_xml test_getset test_trie test_mount test_cap test_stream test_serialize test_split test_internals test_backendhelpers test_type print_info
#check_PROGRAMS    = test_backendhelpers

if VALGRINDTESTS
TESTS_ENVIRONMENT = $(VALGRIND) --quiet --show-reachable=yes --leak-check=yes
endif

EXTRA_DIST = dbelow.c filesys.xml fstab.xml fstab-cmp.xml hosts.xml keyset.c keyset.xml key.xml key-cmp.xml others.c passwd.xml test_script.sh test_validate.sh test_script.sh tests.c tests.h

clean-local:
	rm -rf .kdb
	rm -f *-gen.xml
	rm -f *.gcno *.gcda *.gcno

test_key_SOURCES = test_key.c tests.h tests.c
test_key_LDADD = $(privatelibs) ../src/libelektra/libelektra.a 

test_ks_SOURCES = test_ks.c tests.h tests.c
test_ks_LDADD = $(privatelibs) ../src/libelektra/libelektra.a 

test_kdb_SOURCES = test_kdb.c tests.h tests.c
test_kdb_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_xml_SOURCES = test_xml.c tests.h tests.c
test_xml_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_getset_SOURCES = test_getset.c tests.h tests.c
test_getset_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_trie_SOURCES = test_trie.c tests.h tests.c
test_trie_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_mount_SOURCES = test_mount.c tests.h tests.c
test_mount_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_cap_SOURCES = test_cap.c tests.h tests.c
test_cap_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_stream_SOURCES = test_stream.c tests.h tests.c
test_stream_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_serialize_SOURCES = test_serialize.c tests.h tests.c
test_serialize_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_split_SOURCES = test_split.c tests.h tests.c
test_split_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_internals_SOURCES = test_internals.c tests.h tests.c
test_internals_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_backendhelpers_SOURCES = test_backendhelpers.c tests.h tests.c
test_backendhelpers_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

test_type_SOURCES = test_type.c tests.h tests.c
test_type_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

print_info_SOURCES = print_info.c tests.h tests.c
print_info_LDADD = $(privatelibs) ../src/libelektra/libelektra.a

../src/libelektra/libelektra.a:
	cd ../src/libelektra && $(MAKE) libelektra.a

