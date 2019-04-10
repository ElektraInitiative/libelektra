cat << 'EOF' > dummy.c
#include "simple.actual.h"
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
	elektraSize (elektra, ELEKTRA_TAG_MYFLOATARRAY);

	elektraGetV (elektra, ELEKTRA_TAG_MYFLOATARRAY, 0);
	elektraGet (elektra, ELEKTRA_TAG_PRINT);
	elektraGet (elektra, ELEKTRA_TAG_MYSTRING);
	elektraGet (elektra, ELEKTRA_TAG_MYINT);
	elektraGet (elektra, ELEKTRA_TAG_MYDOUBLE);

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

int main (int argc, const char ** argv)
{
	specloadCheck (argc, argv);

	ElektraError * error = NULL;
	Elektra * elektra = NULL;
	int rc = loadConfiguration (&elektra, &error);

	if (rc == -1)
	{
		fprintf (stderr, "couldn't load config %s", elektraErrorDescription (error));
		elektraErrorReset (&error);
		return EXIT_FAILURE;
	}

	if (rc == 1)
	{
		fprintf (stderr, "unexpected help mode");
		return EXIT_FAILURE;
	}

	callAll (elektra);

	elektraClose (elektra);
	return EXIT_SUCCESS;
}
EOF

cat << 'EOF' > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wpedantic -Wall -Werror")

add_executable (dummy dummy.c simple.actual.c)
target_include_directories (dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
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
rm CMakeLists.txt dummy.c

exit "$res"
