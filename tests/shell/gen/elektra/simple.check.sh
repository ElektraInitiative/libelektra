cat << EOF > CMakeLists.txt
cmake_minimum_required(VERSION 3.0)

add_library (dummy OBJECT simple.actual.c)
target_include_directories(dummy PRIVATE "@CMAKE_BINARY_DIR@/src/include" "@CMAKE_SOURCE_DIR@/src/include")
EOF

mkdir build && cd build

cmake .. -DCMAKE_C_COMPILER="@CMAKE_C_COMPILER@" && cmake --build .
res=$?

cd ..
rm -r build
rm CMakeLists.txt
