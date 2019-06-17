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

	VALUE_CHECK (elektraGetV (elektra, ELEKTRA_TAG_MYFLOATARRAY, 0), 1.1f);
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

	if (rc == 2)
	{
		fprintf (stderr, "unexpected help mode");
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

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wpedantic -Wall -Werror")

add_executable (dummy dummy.c simple.actual.c)
target_include_directories (dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")

foreach (LIB @ElektraCodegen_ALL_LIBRARIES@)
	find_library ("${LIB}_PATH" "${LIB}" HINTS "@CMAKE_BINARY_DIR@/lib")
	target_link_libraries (dummy ${${LIB}_PATH})
endforeach ()
EOF

mkdir build && cd build

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

if [ "$res" = "0" ]; then
	"$KDB" setmeta "user$MOUNTPOINT/myfloatarray" "array" "#0"

	./dummy
	res=$?
	echo "dummy exited with: $res"

	"$KDB" export "$MOUNTPOINT" ni > ~/export.casc.ini
	"$KDB" export "spec$MOUNTPOINT" ni > ~/export.spec.ini
	"$KDB" export "user$MOUNTPOINT" ni > ~/export.user.ini

	if [ "$res" = "0" ] && which valgrind; then
		valgrind --leak-check=full --leak-resolution=high --track-origins=yes --vgdb=no --trace-children=yes ./dummy
		res=$?
		echo "valgrind dummy exited with: $res"
	fi
fi

cd ..
rm -r build
rm CMakeLists.txt dummy.c

exit "$res"
