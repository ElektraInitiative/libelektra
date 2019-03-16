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

cat << EOF > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

add_library (dummy OBJECT enum.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
EOF

mkdir build && cd build

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

cd ..
rm -r build
rm CMakeLists.txt colors.h

exit $res
