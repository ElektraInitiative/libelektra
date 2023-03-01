#!/bin/sh

cat << 'EOF' > dummy.c
#include "simple.actual.h"
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

void callAll (Elektra * elektra)
{
	VALUE_CHECK (elektraSize (elektra, ELEKTRA_TAG_MYFLOATARRAY), 1);

	VALUE_CHECK (elektraGetV (elektra, ELEKTRA_TAG_MYFLOATARRAY, 0), 2.5f);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_PRINT), false);
	VALUE_CHECK (strcmp (elektraGet (elektra, ELEKTRA_TAG_MYSTRING), ""), 0);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_MYINT), 0);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_MYDOUBLE), 0.0);

	ElektraError * error = NULL;

	elektraSetV (elektra, ELEKTRA_TAG_MYFLOATARRAY, 18.4f, &error, 0);
	ERROR_CHECK (ELEKTRA_TAG_MYFLOATARRAY)
	elektraSetV (elektra, ELEKTRA_TAG_MYFLOATARRAY, 120.2f, &error, 1);
	ERROR_CHECK (ELEKTRA_TAG_MYFLOATARRAY)
	elektraSet (elektra, ELEKTRA_TAG_MYDOUBLE, 23.23, &error);
	ERROR_CHECK (ELEKTRA_TAG_MYDOUBLE)
	elektraSet (elektra, ELEKTRA_TAG_MYINT, 11, &error);
	ERROR_CHECK (ELEKTRA_TAG_MYINT)
	elektraSet (elektra, ELEKTRA_TAG_PRINT, true, &error);
	ERROR_CHECK (ELEKTRA_TAG_PRINT)
	elektraSet (elektra, ELEKTRA_TAG_MYSTRING, "test", &error);
	ERROR_CHECK (ELEKTRA_TAG_MYSTRING)
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

add_executable (dummy dummy.c simple.actual.c)
target_include_directories (dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")

foreach (LIB @ElektraCodegen_ALL_LIBRARIES@)
	find_library ("${LIB}_PATH" "${LIB}" HINTS "@CMAKE_BINARY_DIR@/lib")
	target_link_libraries (dummy ${${LIB}_PATH})
endforeach ()
EOF

mkdir build && cd build || exit 1

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

if [ "$res" = "0" ]; then
	"$KDB" meta set "user:$MOUNTPOINT/myfloatarray" "array" "#0"

	./dummy
	res=$?
	echo "dummy exited with: $res"

	"$KDB" export "$MOUNTPOINT" ni > ~/export.casc.ini
	"$KDB" export "spec:$MOUNTPOINT" ni > ~/export.spec.ini
	"$KDB" export "user:$MOUNTPOINT" ni > ~/export.user.ini

	if [ "$res" = "0" ] && command -v valgrind; then
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes ./dummy
		echo "valgrind dummy exited with: $?"
	fi
fi

cd ..
rm -r build
rm CMakeLists.txt dummy.c

exit "$res"
