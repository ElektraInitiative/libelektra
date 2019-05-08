# Tree example

This example application reads its configuration from the KDB using code-generated functions.
It then prints a tree defined inside the KDB.

## Setup

### CMake

```sh
# execute in the current directory or replace $PWD accordingly
DIR=$PWD

mkdir "$DIR/cmake/build" && cd "$DIR/cmake/build"

cmake ..
cmake --build .

cd "$DIR"

sudo kdb mount -R noresolver codegen_tree_example.conf "spec/sw/example/tree/#0/current" specload "app=$DIR/cmake/build/application"
sudo kdb spec-mount "/sw/example/tree/#0/current"
```

### Pkgconfig

```sh
# execute in the current directory or replace $PWD accordingly
DIR=$PWD

cd "$DIR/pkgconfig"

make

cd "$DIR"

sudo kdb mount -R noresolver codegen_tree_example.conf "spec/sw/example/tree/#0/current" specload "app=$DIR/pkgconfig/application"
sudo kdb spec-mount "/sw/example/tree/#0/current"
```

## Running

To run the application, simply execute:

```sh
# execute in the current directory or replace $PWD accordingly
DIR=$PWD

# for CMake
LOC="cmake/build"

# for Pkgconfig
# LOC="pkgconfig"

"$DIR/$LOC/application"
```

## Configuration

The supported KDB configuration is described in `spec.ini`.

The root of the tree defined by `root` must be a reference to one of the node structs defined in
`tree/_`. Each of these structs consists of two parts. First a string key `tree/_/text` that describes
the text printed for this node and second an array `tree/_/children/#` of references to the other node
structs, which are the children of the current node.
