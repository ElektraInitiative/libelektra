# Get Started

This document is intended for developers who want to get started with developing with Elektra.

## Skill requirements

- Operating system

  We recommend a Unix-based operating system to run Elektra (Linux, BSD, macOS) but it's also possible to use Windows which is supported but not yet fully tested.

- Using command-line interface and commands

  The easiest way to compile, install and use Elektra is by using the terminal. We will introduce the basic commands which you will need to run Elektra for the very first time.

- Basic knowledge about git

  Don't panic! [GIT](https://git-scm.com/) is a distributed version control system to track changes of the source code in a project. We will use a single GIT command to get
  the source code of Elektra.

- Basic knowledge about make/cmake

  [make](https://www.gnu.org/software/make/) or [cmake](https://cmake.org/) are used to generate an executable program from the code.
  If you are not used to these tools, it's not a problem, we will introduce them to you in later sections.

- We also need your skill set to improve Elektra

  You can contribute to Elektra to improve the source code, website, documentation, translation etc.

## Software requirements

We need to install some basic tools to run Elektra: cmake, git and essential build tools (make, gcc, and some standard Unix tools;
alternatively [ninja](https://ninja-build.org/) and [clang](https://clang.llvm.org/index.html) are also supported but not described here).
Depending on your Linux distribution use the following commands to install these tools:

```sh
sudo apt-get install cmake git build-essential
```

Or on RPM (Red Hat Package Manager) based systems (like Fedora, openSUSE, CentOS etc.):

```sh
sudo yum install -y cmake git gcc-c++
```

Or on macOS, most of the build tools can be obtained by installing [Xcode](https://developer.apple.com/xcode/). Other required tools may be installed using [brew](https://brew.sh/).
First, install brew as described on their website. Then issue the following command to get cmake to complete the basic requirements:

```sh
brew install cmake git
```

## Installation

If you meet all the software requirements you can get the source code of Elektra by using this command:

```sh
git clone https://github.com/ElektraInitiative/libelektra.git
```

Run the following commands to compile Elektra with non-experimental plugins where your system happens to fulfill the dependencies:

```sh
cd libelektra  #navigate to libelektra
mkdir build  && cd build  #create and navigate to the build directory
cmake ..  # watch output to see if everything needed is included
#  optionally run "ccmake .." to get an overview of the available build settings (needs cmake-curses-gui)
cmake --build . -- -j5
```

Optionally you can also run tests, see [here for more information](/doc/TESTING.md):

```sh
cmake --build . --target run_nokdbtests
```

With these commands you will be able to run the "Hello World!" example, but usually you will need to use some of the [plugins](/src/plugins/README.md), tools and bindings of Elektra.
Please take a look at the more detailed [compiling documentation](/doc/COMPILE.md). After you completed building Elektra on your own, you can execute these commands to install Elektra
(please check the [installation documentation](/doc/INSTALL.md)for the many available packages):

```sh
sudo make install
sudo ldconfig #optional: check installation documentation for more information
```

[Installation documentation](/doc/INSTALL.md) contains further information about available packages.

Optionally you can also run tests to verify the installed Elektra, see [here for more information](/doc/TESTING.md):

```sh
kdb run_nokdbtests
```

## Hello first steps!

This section attempts to give a brief introduction on how to use the key database of Elektra. The key database is modified and queried by using the `kdb` tool.

Let's look into an example:

```sh
sudo kdb mount "hello.spec.ni" "spec:/example/hello" ni
```

This first command shows one of the core concepts: In Elektra you mount a file into the key database by specifying a mountpoint.
In this example we mount the file `hello.spec.ni` that shall be our configuration specification to the mountpoint `/example/hello`
in the `spec` [namespace](/doc/tutorials/namespaces.md) (which is the namespace used for specifications).
We could also say that we persistently mount a new _backend_. The last parameter `ni` specifies that we use the [ni plugin](/src/plugins/ni/README.md)
to write in the ni format (a specialised variant of INI) to the file. This can also be omitted in which case the less human-friendly [dump plugin](/src/plugins/dump/) is used.

Since we did not denote an absolute file path it depends on the namespace where the actual file is stored.
However, we can always retrieve the location of a file with the `file` subcommand, e.g., `kdb file "spec:/example/hello"`.

Now let's add some data to our specification:

```sh
sudo kdb set "spec:/example/hello/what" ""
```

As you see above we extended the the mountpoint `/example/hello` with `/what`. Our specification now expects (but not requires) a `what` key in the configuration. (If you want to actually require it you need to set the metakey `meta:/require`, but more about metakeys below.)
Note that the value is empty since we just want to specify the key.

Now let's show the actual power of Elektra by setting metadata to our specification:

```sh
sudo kdb meta-set "spec:/example/hello/what" default World
```

The metakey `default` (having the value `World`) is now associated with the key `spec:/example/hello/what`.
It should be noted that the previous setting of `what` is in this scenario redundant and could be omitted because this is done implicitly.

Now let's mount our specification:

```sh
sudo kdb meta-set "spec:/example/hello" mountpoint "hello.ni"
sudo kdb spec-mount "/example/hello" ni
```

As you see above, we first specify a mountpoint for our configuration (which is just metadata of the parent key `spec:/example/hello`).
Afterwards we use the `spec-mount` subcommand to mount a new backend by a previously mounted specification. Note that there shall not be a namespace given, because `spec-mount` creates mountpoints for all namespaces (except `spec` of course).

Now let's enjoy some configuration magic:

```sh
kdb get "/example/hello/what"
```

We notice even if we did not set a value for the key `/example/hello/what` we still get `World` back since this is specified as default value.
Therein lies the power of Elektra: We can set metadata (like [types](/src/plugins/type/README.md), [RegEx](/src/plugins/validation/README.md) or [date](/src/plugins/date/README.md) validation, etc.) that gets handled by plugins.

If we want to set a custom value we can do it this way:

```sh
kdb set "user:/example/hello/what" "first few steps"
```

Now a `kdb get /example/hello/what` would return `first few steps`.

Note that when we set a value the namespace of the key should be specified explicitly.
If we don't give a proper namespace (like in the get example above) a so-called [cascading lookup](/doc/tutorials/cascading.md) is done.
However, cascading lookups during `kdb set` only work, if there is already an existing value (i.e. there is no ambiguity which key we want to set).

## Further Reading

Start with your very first Elektra application in C and follow these steps:
[Hello World!](/doc/tutorials/hello-elektra.md)

Modify the [website](https://www.libelektra.org/home) with the following [tutorial](/src/tools/website/). The website is build with angular and hosted with grunt.

Also take a look at the [webui](/src/tools/webui/).
