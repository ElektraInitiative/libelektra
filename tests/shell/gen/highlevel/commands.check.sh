#!/bin/sh

cat << 'EOF' > dummy.c
#include "commands.actual.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define ERROR_CHECK(tag)                                                                                                               \
	if (error != NULL)                                                                                                                 \
	{                                                                                                                                  \
		elektraErrorReset (&error);                                                                                                    \
		elektraClose (elektra);                                                                                                        \
		fprintf (stderr, "couldn't set %s", #tag);                                                                                     \
		exit(EXIT_FAILURE);                                                                                                            \
	}

#define VALUE_CHECK(expr, expected)                                                                                                    \
	if ((expr) != (expected))                                                                                                          \
	{                                                                                                                                  \
		elektraClose (elektra);                                                                                                        \
		fprintf (stderr, "value wrong %s\n", #expr);                                                                                   \
		exit(EXIT_FAILURE);                                                                                                            \
	}

static void fatalErrorHandler (ElektraError * error)
{
	fprintf (stderr, "FATAL ERROR: %s\n", elektraErrorDescription (error));
	elektraFree (error);
	exit (EXIT_FAILURE);
}

int commandKdb (Elektra * elektra, kdb_boolean_t terminal, void * userData)
{
	kdb_boolean_t printversion = elektraGet (elektra, ELEKTRA_TAG_PRINTVERSION);
	
	printf ("commandKdb called %s, printversion: %d\n", terminal ? "last" : "in the middle", printversion ? 1 : 0);
	return 0;
}

int commandKdbGet (Elektra * elektra, kdb_boolean_t terminal, void * userData)
{
	kdb_boolean_t verbose = elektraGet (elektra, ELEKTRA_TAG_GET_VERBOSE);
	kdb_long_t maxLength = elektraGet (elektra, ELEKTRA_TAG_GET_MAXLENGTH);
	const char * keyname = elektraGet (elektra, ELEKTRA_TAG_GET_KEYNAME);
	
	printf ("commandKdbGet called %s, verbose: %d, maxLength: " ELEKTRA_LONG_F ", keyname: %s\n", terminal ? "last" : "in the middle", verbose ? 1 : 0, maxLength, keyname);
	return 0;
}

int commandKdbGetMeta (Elektra * elektra, kdb_boolean_t terminal, void * userData)
{
	kdb_boolean_t verbose = elektraGet (elektra, ELEKTRA_TAG_GET_META_VERBOSE);
	const char * keyname = elektraGet (elektra, ELEKTRA_TAG_GET_META_KEYNAME);
	const char * metaname = elektraGet (elektra, ELEKTRA_TAG_GET_META_METANAME);

	printf ("commandKdbGetMeta called %s, verbose: %d, keyname: %s, metaname: %s\n",
		terminal ? "last" : "in the middle", verbose ? 1 : 0, keyname, metaname);
	return 0;
}

int commandKdbSet (Elektra * elektra, kdb_boolean_t terminal, void * userData)
{
	const char * keyname = elektraGet (elektra, ELEKTRA_TAG_SETTER_KEYNAME);
	const char * value = elektraGet (elektra, ELEKTRA_TAG_SETTER_VALUE);

	printf ("commandKdbSet called %s, keyname: %s, value: %s\n", terminal ? "last" : "in the middle", keyname, value);
	return 0;
}


void callAll (Elektra * elektra)
{
	runCommands (elektra, NULL);
}

extern const char * const * environ;

int main (int argc, const char * const * argv)
{
	exitForSpecload (argc, argv);

	ElektraError * error = NULL;
	Elektra * elektra = NULL;
	int rc = loadConfiguration (&elektra, argc, argv, environ, &error);

	if (rc == -1)
	{
		fprintf (stderr, "couldn't load config %s\n", elektraErrorDescription (error));
		elektraErrorReset (&error);
		return EXIT_FAILURE;
	}

	if (rc == 1)
	{
		fprintf (stderr, "unexpected help mode");
		elektraClose (elektra);
		return EXIT_FAILURE;
	}

	elektraFatalErrorHandler (elektra, fatalErrorHandler);

	callAll (elektra);

	elektraClose (elektra);
	return EXIT_SUCCESS;
}
EOF

cat << 'EOF' > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} @C_FLAG_32BIT@ -std=c99 -Wpedantic -Wall -Werror")

add_executable (dummy dummy.c commands.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")

foreach (LIB @ElektraCodegen_ALL_LIBRARIES@)
	find_library ("${LIB}_PATH" "${LIB}" HINTS "@CMAKE_BINARY_DIR@/lib")
	target_link_libraries (dummy ${${LIB}_PATH})
endforeach ()
EOF

mkdir build && cd build || exit 1

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

if [ "$res" = "0" ]; then
	./dummy
	res=$?
	echo "dummy exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy
		echo "valgrind dummy exited with: $res"
	fi

	res=$((res != 0))
fi

if [ "$res" = 0 ]; then
	./dummy -v get -v ab cd
	res=$?
	echo "dummy -v get -v ab cd exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy -v get -v ab cd
		echo "valgrind dummy -v get -v ab cd exited with: $?"
	fi

	res=$((res != 0))
fi

if [ "$res" = "0" ]; then
	./dummy -v get --max-length=10 ab cd
	res=$?
	echo "dummy -v get --max-length=10 ab cd exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy -v get --max-length=10 ab cd
		echo "valgrind dummy -v get -v ab cd exited with: $?"
	fi

	res=$((res != 0))
fi

if [ "$res" = "0" ]; then
	./dummy -v get --max-length=notanint ab cd
	res=$?
	echo "dummy -v get --max-length=notanint ab cd exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy -v get --max-length=notanint ab cd
		echo "valgrind dummy -v get -v ab cd exited with: $?"
	fi

	res=$((res == 0))
fi

if [ "$res" = "0" ]; then
	./dummy get meta -v a b
	res=$?
	echo "dummy get meta -v a b exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy get meta -v a b
		echo "valgrind dummy get meta -v a b exited with: $?"
	fi

	res=$((res != 0))
fi

if [ "$res" = "0" ]; then
	./dummy set -- def -2
	res=$?
	echo "dummy set def -2 exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy set def -2
		echo "valgrind dummy set def -2 exited with: $?"
	fi

	res=$((res != 0))
fi

if [ "$res" = "0" ]; then
	./dummy abc -v def
	res=$?
	echo "dummy abc -v def exited with: $res"

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes -- ./dummy abc -v def
		echo "valgrind dummy abc -v def exited with: $?"
	fi

	res=$((res != 0))
fi

cd ..
rm -r build
rm CMakeLists.txt dummy.c

exit "$res"
