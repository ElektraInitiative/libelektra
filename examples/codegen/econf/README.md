## Setup

### CMake

```sh
# execute in the current directory or replace $PWD accordingly
DIR=$PWD

mkdir "$DIR/cmake/build" && cd "$DIR/cmake/build"

cmake ..
cmake --build .

cd "$DIR"

kdb mount codegen_econf_example.conf "spec/sw/example/econf/#0/current" specload "app=$DIR/build/application"

```
