#!/bin/sh

cat << 'EOF' > colors.h
#ifndef COLORS_H
#define COLORS_H
typedef enum {
	EXISTING_COLORS_CYAN = 0,
	EXISTING_COLORS_MAGENTA = 1,
	EXISTING_COLORS_YELLOW = 2
} ExistingColors;

#define NO_VALUE 0
#endif // COLORS_H
EOF

cat << 'EOF' > dummy.c
#include "enum.actual.h"
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
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_DISJOINTED), ELEKTRA_ENUM_DISJOINTED_BLACK);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_EXISTINGGENTYPE), EXISTING_COLORS_CYAN);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_GENTYPE), COLORS_BLUE);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_GENTYPE2), COLORS_RED);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_MYENUM), ELEKTRA_ENUM_MYENUM_BLUE);

	ElektraError * error = NULL;

	elektraSet (elektra, ELEKTRA_TAG_DISJOINTED, ELEKTRA_ENUM_DISJOINTED_WHITE, &error);
	ERROR_CHECK (ELEKTRA_TAG_DISJOINTED)
	elektraSet (elektra, ELEKTRA_TAG_EXISTINGGENTYPE, EXISTING_COLORS_YELLOW, &error);
	ERROR_CHECK (ELEKTRA_TAG_EXISTINGGENTYPE)
	elektraSet (elektra, ELEKTRA_TAG_GENTYPE, COLORS_GREEN, &error);
	ERROR_CHECK (ELEKTRA_TAG_GENTYPE)
	elektraSet (elektra, ELEKTRA_TAG_GENTYPE2, COLORS_GREEN, &error);
	ERROR_CHECK (ELEKTRA_TAG_GENTYPE2)
	elektraSet (elektra, ELEKTRA_TAG_MYENUM, ELEKTRA_ENUM_MYENUM_BLUEISH, &error);
	ERROR_CHECK (ELEKTRA_TAG_MYENUM)
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

add_executable (dummy dummy.c enum.actual.c)
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
		valgrind --error-exitcode=2 --show-leak-kinds=all --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes ./dummy
		echo "valgrind dummy exited with: $?"
	fi
fi

cd ..
rm -r build
rm CMakeLists.txt colors.h dummy.c

exit "$res"
