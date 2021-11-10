# Get Started

- Skill requirements

  - Operating system

    We recommend a Unix-based operating system to run Elektra (Linux, BSD, macOS) but it's also possible to use Windows which is supported but not yet fully tested.

  - Using command-line interface and commands

    The easiest way to compile, install and use Elektra is by using the terminal. We will introduce the basic commands which you will need to run Elektra for the very first time.

  - Basic knowledge about git

    Dont panic! [GIT](https://git-scm.com/) is a distributed version constrol system to track changes of the source code in a project. We will use a single command of GIT to get
    the source code of Elektra.

  - Basic knowledge about make/cmake

    [make](https://www.gnu.org/software/make/) or [cmake](https://cmake.org/) are used to generate an executable program from the code. If you are not used to these tools its no problem, we will introduce them to you in later sections.

  - We need also your skill set to improve Elektra

    You can contribute to Elektra to improve the source code, website, documentation, translation etc.

- Software requirements

  We need to install some basic tools to run Elektra: cmake , git , and essential build tools (make, gcc, and some standard Unix tools; alternatively [ninja](https://ninja-build.org/) and [clang](https://clang.llvm.org/index.html) are also supported but not described here). Depending on your linux distribution use following commands to install these tools:

  ```sh
  sudo apt-get install cmake git build-essential
  ```

  Or on RPM (Red Hat Package Manager) based systems (like Fedora, openSUSE, CentOS etc.):

  ```sh
  sudo yum install -y cmake git gcc-c++
  ```

  Or on macOS, most of the build tools can be obtained by installing [Xcode](https://developer.apple.com/xcode/). Other required tools may be installed using [brew ](https://brew.sh/). First install brew as described on their website. Then issue the following command to get cmake to complete the basic requirements:

  ```sh
  brew install cmake git
  ```

- Installation

  If you meet all of the software requirements you can get the source code of Elektra by using this command:

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

  Optionally you can also run tests on your build, see [here for more information](/doc/TESTING.md):
  Note that if you followed the guide you are still inside the libelektra/build directory.

  ```sh
  cmake --build . --target run_nokdbtests #run tests on your build
  ```

  With these commands you will be able to run the "Hello World!" example but usually you will need to use some of the [plugins](/src/plugins/README.md), tools and bindings of Elektra. Please take a look at the more detailed [compiling documentation](/doc/COMPILE.md). After you completed building Elektra on your own, you can execute these commands to install Elektra (please check the [installation documentation](/doc/INSTALL.md) for the many available packages):

  ```sh
  sudo make install
  sudo ldconfig #optional: check installation documentation for more information
  ```

  [Installation documentation](/doc/INSTALL.md) contains further information about available packages.

  Optionally you can also run tests to verify the installed Elektra, see [here for more information](/doc/TESTING.md):

  ```sh
  kdb run_nokdbtests
  ```

- Hello World!

  Start with your very first Elektra application in C and follow these steps:
  [Hello World!](/doc/tutorials/hello-elektra.md)
