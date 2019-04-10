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

#define ERROR_CHECK(tag)                                                                                                               \
	if (error != NULL)                                                                                                                 \
	{                                                                                                                                  \
		elektraErrorReset (&error);                                                                                                    \
		elektraClose (elektra);                                                                                                        \
		fprintf (stderr, "couldn't set %s", #tag);                                                                                     \
		exit(EXIT_FAILURE);                                                                                                            \
	}

void callAll (Elektra * elektra)
{
	elektraGet (elektra, ELEKTRA_TAG_DISJOINTED);
	elektraGet (elektra, ELEKTRA_TAG_EXISTINGGENTYPE);
	elektraGet (elektra, ELEKTRA_TAG_GENTYPE);
	elektraGet (elektra, ELEKTRA_TAG_GENTYPE2);
	elektraGet (elektra, ELEKTRA_TAG_MYENUM);

	ElektraError * error = NULL;

	elektraSet (elektra, ELEKTRA_TAG_DISJOINTED, ELEKTRA_ENUM_DISJOINTED_BLACK, &error);
	ERROR_CHECK (ELEKTRA_TAG_DISJOINTED)
	elektraSet (elektra, ELEKTRA_TAG_EXISTINGGENTYPE, EXISTING_COLORS_YELLOW, &error);
	ERROR_CHECK (ELEKTRA_TAG_EXISTINGGENTYPE)
	elektraSet (elektra, ELEKTRA_TAG_GENTYPE, COLORS_GREEN, &error);
	ERROR_CHECK (ELEKTRA_TAG_GENTYPE)
	elektraSet (elektra, ELEKTRA_TAG_GENTYPE2, COLORS_GREEN | COLORS_RED, &error);
	ERROR_CHECK (ELEKTRA_TAG_GENTYPE2)
	elektraSet (elektra, ELEKTRA_TAG_MYENUM, ELEKTRA_ENUM_MYENUM_BLUEISH, &error);
	ERROR_CHECK (ELEKTRA_TAG_MYENUM)
}

int main (int argc, const char ** argv)
{
	specloadCheck (argc, argv);

	ElektraError * error = NULL;
	Elektra * elektra = NULL;
	int rc = loadConfiguration (&elektra, &error);

	if (rc == -1)
	{
		elektraErrorReset (&error);
		return EXIT_FAILURE;
	}

	if (rc == 1)
	{
		printHelpMessage (NULL, NULL);
		return EXIT_SUCCESS;
	}

	callAll (elektra);

	elektraClose (elektra);
	return EXIT_SUCCESS;
}
EOF

cat << 'EOF' > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wpedantic -Wall -Werror")

add_executable (dummy dummy.c enum.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
target_link_libraries (dummy "@CMAKE_BINARY_DIR@/lib/libelektra-highlevel.so" "@CMAKE_BINARY_DIR@/lib/libelektra-opts.so"
							 "@CMAKE_BINARY_DIR@/lib/libelektra-invoke.so" "@CMAKE_BINARY_DIR@/lib/libelektra-kdb.so"
							 "@CMAKE_BINARY_DIR@/lib/libelektra-ease.so" "@CMAKE_BINARY_DIR@/lib/libelektra-core.so")
EOF

mkdir build && cd build

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

if [ "$res" = "0" ]; then
	./dummy
	if [ "$res" = "0" ]; then
		echo "dummy exited with: $res"
	fi
	res=$?
fi

cd ..
rm -r build
rm CMakeLists.txt colors.h dummy.c

exit "$res"
