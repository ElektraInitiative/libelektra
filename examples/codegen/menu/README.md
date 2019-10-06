# Menu example

This example application reads its configuration from the KDB using code-generated functions.
It allows navigating through a menu structure.

Each menu can have sub-menus and an attached command. The user can either choose to open a sub-menu or to execute the
attached command.

## Setup

### CMake

```sh
# execute in the current directory or replace $PWD accordingly
mkdir "$PWD/cmake/build" && cd "$PWD/cmake/build"

cmake ..
cmake --build .

sudo kdb mount -R noresolver codegen_menu_example.conf "spec/sw/example/menu/#0/current" specload "app=$PWD/application"
sudo kdb spec-mount "/sw/example/menu/#0/current"
```

### Pkgconfig

```sh
# execute in the current directory or replace $PWD accordingly
cd "$PWD/pkgconfig"

make

sudo kdb mount -R noresolver codegen_menu_example.conf "spec/sw/example/menu/#0/current" specload "app=$PWD/application"
sudo kdb spec-mount "/sw/example/menu/#0/current"
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

The first menu to be displayed is defined by `main`. It must be a reference to one of the menu structs defined in
`menu/_`. Each of these structs consists of three parts:

- A string key `menu/_/name` that sets the name of this menu.
- Another string key `menu/_/command` to set the attached command for this menu.
- An array `menu/_/children/#` of references to the other menu structs, which are the sub-menus of the current menu.

### Example Configuration

Setting up these menu structures is a bit complicated, but here is an example to get you started:

```sh
kdb meta-set "user/sw/example/menu/#0/current/menu" "array" "#4"
kdb set -N user "/sw/example/menu/#0/current/menu/#0/name" "Main Menu"
kdb set -N user "/sw/example/menu/#0/current/menu/#1/name" "Menu 1"
kdb set -N user "/sw/example/menu/#0/current/menu/#2/name" "Menu 2"
kdb set -N user "/sw/example/menu/#0/current/menu/#3/name" "Menu 2.1"
kdb set -N user "/sw/example/menu/#0/current/menu/#4/name" "Menu 2.2"

kdb set -N user "/sw/example/menu/#0/current/menu/#1/command" 'echo "Hello from Menu 1"'
kdb set -N user "/sw/example/menu/#0/current/menu/#2/command" 'echo "Hello from Menu 2"'
kdb set -N user "/sw/example/menu/#0/current/menu/#3/command" 'echo "Hello from Menu 2.1"'
kdb set -N user "/sw/example/menu/#0/current/menu/#4/command" 'echo "Hello from Menu 2.2"'

kdb meta-set "user/sw/example/menu/#0/current/menu/#0/children" "array" "#1"
kdb set -N user "/sw/example/menu/#0/current/menu/#0/children/#0" "@/menu/#1"
kdb set -N user "/sw/example/menu/#0/current/menu/#0/children/#1" "@/menu/#2"

kdb meta-set "user/sw/example/menu/#0/current/menu/#2/children" "array" "#1"
kdb set -N user "/sw/example/menu/#0/current/menu/#2/children/#0" "@/menu/#3"
kdb set -N user "/sw/example/menu/#0/current/menu/#2/children/#1" "@/menu/#4"

kdb set "user/sw/example/menu/#0/current/main" "@/menu/#0"
```

The shell script above sets up this simple menu structure:

Main Menu:

- Menu 1: `echo "Hello from Menu 1"`
- Menu 2: `echo "Hello from Menu 2"`
  - Menu 2.1: `echo "Hello from Menu 2.1"`
  - Menu 2.2: `echo "Hello from Menu 2.2"`
