cat << EOF > colors.h
#ifndef COLORS_H
#define COLORS_H
typedef enum {
	EXISTING_COLORS_ANY = 0
} ExistingColors;

int elektraKeyToEnumExistingColors (const Key * key, ExistingColors * variable);
char * elektraEnumExistingColorsToString (ExistingColors value);

#define NO_VALUE 0
#endif // COLORS_H
EOF

"@CMAKE_C_COMPILER@" -c enum.actual.c -I "@CMAKE_BINARY_DIR@/src/include" -I "@CMAKE_SOURCE_DIR@/src/include" -o enum.actual.o && rm enum.actual.o

res=$?

rm colors.h

exit $res

