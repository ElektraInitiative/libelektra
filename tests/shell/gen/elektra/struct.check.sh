cat << 'EOF' > dummy.c
#include "struct.actual.h"
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
	ElektraStructMystruct mystruct;
	elektraGet2 (elektra, &mystruct, ELEKTRA_TAG_MYSTRUCT);

	Person * adam = elektraGetV (elektra, ELEKTRA_TAG_PERSON, "adam");
	Person * p0 = elektraGetV (elektra, ELEKTRA_TAG_PEOPLE, 0);

	ElektraError * error = NULL;

	elektraSet (elektra, ELEKTRA_TAG_MYSTRUCT, &mystruct, &error);
	elektraSetV (elektra, ELEKTRA_TAG_PERSON, p0, &error, "adam");
	elektraSetV (elektra, ELEKTRA_TAG_PEOPLE, "../../person/adam", &error, 0);
	elektraSetV (elektra, ELEKTRA_TAG_PEOPLE, "../../person/adam", &error, 1);

	ELEKTRA_STRUCT_FREE (StructPerson) (&adam);
	ELEKTRA_STRUCT_FREE (StructPerson) (&p0);
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

add_executable (dummy dummy.c struct.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
link_directories ("@CMAKE_BINARY_DIR@/lib")
target_link_libraries (dummy elektra-highlevel elektra-opts elektra-invoke elektra-kdb elektra-ease elektra-core)
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
