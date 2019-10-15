# EConf Example

This example application reads its configuration from the KDB using code-generated functions.
It then prints an EditorConfig file created from the configuration inside the KDB.

## Setup

### CMake

```sh
# execute in the current directory or replace $PWD accordingly
mkdir "$PWD/cmake/build" && cd "$PWD/cmake/build"

cmake ..
cmake --build .

sudo kdb mount -R noresolver codegen_econf_example.conf "spec/sw/example/econf/#0/current" specload "app=$PWD/application"
sudo kdb spec-mount "/sw/example/econf/#0/current"
```

### Pkgconfig

```sh
# execute in the current directory or replace $PWD accordingly
cd "$PWD/pkgconfig"

make

sudo kdb mount -R noresolver codegen_econf_example.conf "spec/sw/example/econf/#0/current" specload "app=$PWD/application"
sudo kdb spec-mount "/sw/example/econf/#0/current"
```

## Running

To run the application, simply execute:

### CMake

```sh
# execute in the current directory or replace $PWD accordingly
"$PWD/cmake/build/application"
```

### Pkgconfig

```sh
# execute in the current directory or replace $PWD accordingly
# "$PWD/pkgconfig/application"
```

## Configuration

The supported KDB configuration is described in `spec.ini`.

EditorConfig uses sections to match file patterns. In the KDB config, for each such section you should
create an entry in the array `format/#`. The pattern used to match files is given in `format/#/pattern`.

The other keynames used are similar to the keys used by EditorConfig, so they should be self-explanatory.
The values however are sometimes different, to fit Elektra's type system:

- the general `unset` option is not supported everywhere
- `format/#/indent/size` uses `0` instead of `tab`
- `format/#/tabwidth` allows `0`, but this is equivalent to `unset`.
- `1` and `0` are used for booleans instead of `true`/`false`
- `format/#/linelength` uses `0` instead of `off`
