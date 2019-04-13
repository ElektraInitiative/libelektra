cat << 'EOF' > dummy.c
#include "context.actual.h"
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

#define VALUE_CHECK(expr, expected) if (expr != expected) exit(EXIT_FAILURE);

static void fatalErrorHandler (ElektraError * error)
{
	fprintf (stderr, "FATAL ERROR: %s\n", elektraErrorDescription (error));
	elektraFree (error);
	exit (EXIT_FAILURE);
}

void callAll (Elektra * elektra)
{
	elektraContextSet (elektra, ELEKTRA_CONTEXT_DATE, "date");
	elektraContextSet (elektra, ELEKTRA_CONTEXT_PROFILE, "profile");
	elektraContextSet (elektra, ELEKTRA_CONTEXT_USER_NAME, "username");

	VALUE_CHECK (elektraSizeV (elektra, ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN, "abc"), 1);

	VALUE_CHECK (elektraGetV (elektra, ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN, "abc", 0), 15);
	VALUE_CHECK (elektraGet (elektra, ELEKTRA_TAG_KEY_DATE_CHILD), true);
	VALUE_CHECK (strcmp(elektraGet (elektra, ELEKTRA_TAG_KEY_USER__NAME_CHILD), "uchild"), 0);

	ElektraError * error = NULL;

	elektraSetV (elektra, ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN, 18, &error, "abc", 0);
	ERROR_CHECK (ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN)
	elektraSetV (elektra, ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN, 18, &error, "def", 1);
	ERROR_CHECK (ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN)
	elektraSet (elektra, ELEKTRA_TAG_KEY_DATE_CHILD, false, &error);
	ERROR_CHECK (ELEKTRA_TAG_KEY_DATE_CHILD)
	elektraSet (elektra, ELEKTRA_TAG_KEY_USER__NAME_CHILD, "test", &error);
	ERROR_CHECK (ELEKTRA_TAG_KEY_USER__NAME_CHILD)
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

	elektraFatalErrorHandler (elektra, fatalErrorHandler);

	callAll (elektra);

	elektraClose (elektra);
	return EXIT_SUCCESS;
}
EOF

cat << 'EOF' > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wpedantic -Wall -Werror")

add_executable (dummy dummy.c context.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
target_link_libraries (dummy "@CMAKE_BINARY_DIR@/lib/libelektra-highlevel.so" "@CMAKE_BINARY_DIR@/lib/libelektra-opts.so"
							 "@CMAKE_BINARY_DIR@/lib/libelektra-invoke.so" "@CMAKE_BINARY_DIR@/lib/libelektra-kdb.so"
							 "@CMAKE_BINARY_DIR@/lib/libelektra-ease.so" "@CMAKE_BINARY_DIR@/lib/libelektra-core.so")
EOF

mkdir build && cd build

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

if [ "$res" = "0" ]; then
	"$KDB" setmeta "user$MOUNTPOINT/key/profile/child/abc/grandchildren" "array" "#0"
	"$KDB" set "user$MOUNTPOINT/key/profile/child/abc/grandchildren/#0" "15"
	"$KDB" set "user$MOUNTPOINT/key/date/child" "1"
	"$KDB" set "user$MOUNTPOINT/key/username/child" "uchild"

	./dummy
	res=$?
	echo "dummy exited with: $res"

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
